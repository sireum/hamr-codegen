// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.MicrokitFinalizePlugin
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.KiBytesToHex
import org.sireum.hamr.codegen.microkit.util.{MemoryMap, MicrokitUtil, ProtectionDomain, SystemDescription, VirtualMachine}
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object SystemDescriptionProvider_MCS {
  val metaPy: String = "meta.py"
}

@datatype class SystemDescriptionProvider_MCS extends MicrokitFinalizePlugin {

  val name: String = "SystemDescriptionProvider_MSC"

  @strictpure def hasHandled(store: Store): B = store.contains(name)

  @pure override def canFinalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return !reporter.hasError &&
      options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      MicrokitUtil.isMCS(options, symbolTable.rootSystem) &&
      SystemDescriptionProviderPlugin.getMSDs(store).nonEmpty &&
      !hasHandled(store)
  }

  override def finalizeMicrokit(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + name ~> BoolValue(T)
    var resources = ISZ[Resource]()

    val msds: Map[String, SystemDescription] = SystemDescriptionProviderPlugin.getMSDs(localStore)
    for (msd <- msds.values) {
      val protectionDomains = msd.protectionDomains
      val schedulingDomains = msd.schedulingDomains
      val memoryRegions = msd.memoryRegions

      var partitionProtectionDomainsMons: ISZ[ST] = ISZ()

      val channelsST: ISZ[ST] = for (c <- msd.channels) yield st"sdf.add_channel(${c.prettySdfgenST})"

      var monChannels: ISZ[ST] = ISZ()
      var vmSetupST: ISZ[ST] = ISZ()

      var memMaps: Map[String, ISZ[(MemoryMap, String)]] = Map.empty

      def processProtectionDomain(pd: ProtectionDomain, parent: String): Unit = {
        if (ops.StringOps(pd.name).endsWith("MON")) {
          monChannels = monChannels :+ st"channel_${pd.name} = ${pd.schedulingDomain.get}"
        }

        for (map <- pd.memMaps) {
          val existing = memMaps.getOrElse(map.memoryRegion, ISZ())
          memMaps = memMaps + map.memoryRegion ~> (existing :+ (map, pd.name))
        }
        for (child <- pd.children) {
          child match {
            case vm: VirtualMachine =>
              for (map <- vm.memMaps) {
                if (!memMaps.contains(map.memoryRegion)) {
                  memMaps = memMaps + map.memoryRegion ~> ISZ()
                }
              }
            case _ =>
          }
        }

        val idOpt: String = pd.id match {
          case Some(v) => s", child_id=$v"
          case _ => ""
        }

        var irqStatements: ISZ[ST] = ISZ()
        for (irq <- pd.irqs) {
          irqStatements = irqStatements :+
            st"""${pd.name}.add_irq(IrqConventional(irq=${irq.irq}, id=${irq.id}))"""
        }

        partitionProtectionDomainsMons = partitionProtectionDomainsMons :+
          st"""${pd.name} = ${pd.prettySdfgenProtectionDomainST}
              |${parent}.add_child_pd(${pd.name}${idOpt})
              |${(irqStatements, "\n")}"""

        for(child <- pd.children) {
          child match {
            case childPd: ProtectionDomain =>
              processProtectionDomain(childPd, pd.name)
            case vm: VirtualMachine =>
              val vmVar = s"${vm.name}_vm"
              var vmStatements: ISZ[ST] = ISZ(
                st"""${vmVar} = VirtualMachine("${vm.name}", [VirtualMachine.Vcpu(id=${vm.vcpuId})])""")
              for (map <- vm.memMaps) {
                val addr = KiBytesToHex(map.vaddrInKiBytes)
                val cachedOpt: String = map.cached match {
                  case Some(v) => s", cached=${if (v) "True" else "False"}"
                  case _ => ""
                }
                val setvarOpt: String = map.varAddr match {
                  case Some(v) => s""", setvar_vaddr="$v""""
                  case _ => ""
                }
                vmStatements = vmStatements :+
                  st"""${vmVar}.add_map(Map(${map.memoryRegion}, $addr, perms="${map.permsPettyPrint}"$cachedOpt$setvarOpt))"""
              }
              vmStatements = vmStatements :+ st"""${pd.name}.set_virtual_machine(${vmVar})"""
              vmSetupST = vmSetupST ++ vmStatements
            case _ =>
          }
        }
      }

      for (pd <- protectionDomains) {
        processProtectionDomain(pd, "scheduler")
      }

      var schedulePairs: ISZ[ST] = ISZ()
      var schedule: ISZ[ST] = ISZ()
      var seenPairNames: Set[String] = Set.empty
      for (sd <- schedulingDomains) {
        val channelName: String = if (sd.componentName == "pad") "0" else s"channel_${sd.componentName}"
        val varName: String = s"ts_${sd.componentName}"
        if (!seenPairNames.contains(varName)) {
          schedulePairs = schedulePairs :+ st"$varName = ($channelName, ${sd.length}, ${if (sd.isUserPartition) "True" else "False"})"
          seenPairNames = seenPairNames + varName
        }
        schedule = schedule :+ st"$varName"
      }

      var memoryRegionsST: ISZ[ST] = ISZ()
      var mapsSt: ISZ[ST] = ISZ()
      var pageSizeMappingsST: ISZ[ST] = ISZ()
      // Plugin-contributed Python code (e.g. sched_state/sched_schedule region creation
      // by the runtime monitor plugin).  Template-managed regions have their MemoryRegion
      // creation emitted here; the standard loop only emits their PD add_map calls.
      val templateManagedMapsSt: ISZ[ST] = msd.templateContributions
      for (m <- memoryRegions) {
        // Skip regions that no PD in this SD variant maps (e.g. a region belonging to a
        // stripped monitor PD is present in memoryRegions but absent from the normal SD's PDs).
        // Template-managed regions (isTemplateManaged=T) are created directly in the meta.py
        // template; the loop only emits their PD add_map calls, not the MemoryRegion creation.
        memMaps.get(m.name) match {
          case Some(maps) =>
            if (!m.isTemplateManaged) {
              val paddrOpt: String = m.physicalAddressInKiBytes match {
                case Some(p) => s", paddr=${KiBytesToHex(p)}"
                case _ => ""
              }
              memoryRegionsST = memoryRegionsST :+
                st"""${m.name} = MemoryRegion(sdf, "${m.name}", ${KiBytesToHex(m.sizeInKiBytes)}$paddrOpt)
                    |sdf.add_mr(${m.name})"""
              m.pageSizeInKiBytes match {
                case Some(ps) =>
                  pageSizeMappingsST = pageSizeMappingsST :+
                    st""""${m.name}": "${KiBytesToHex(ps)}""""
                case _ =>
              }
            }
            for (map <- maps) {
              val addr = KiBytesToHex(map._1.vaddrInKiBytes)
              val setvarOpt: String = map._1.varAddr match {
                case Some(v) => s""", setvar_vaddr="$v""""
                case _ => ""
              }
              mapsSt = mapsSt :+
                st"""${map._2}.add_map(Map(${m.name}, $addr, perms="${map._1.permsPettyPrint}"$setvarOpt))"""
            }
          case _ =>
        }
      }

      val vmSectionOpt: ST =
        if (vmSetupST.nonEmpty)
          st"""#######################################
              |# VMMs
              |#######################################
              |${(vmSetupST, "\n")}"""
        else st""

      val xmlImportOpt: ST =
        if (pageSizeMappingsST.nonEmpty) st"""import xml.etree.ElementTree as ET
                                              |"""
        else st""

      val renderST: ST =
        if (pageSizeMappingsST.nonEmpty)
          st"""# Post-process sdf.render() to add page_size attributes not yet
              |# supported by sdfgen's Python API.
              |def add_page_size(xml_str, mappings):
              |    root = ET.fromstring(xml_str)
              |    for mr in root.iter('memory_region'):
              |        name = mr.get('name')
              |        if name in mappings:
              |            mr.set('page_size', mappings[name])
              |    ET.indent(root, space='  ')
              |    return ET.tostring(root, encoding='unicode', xml_declaration=True)
              |
              |page_size_mappings = {
              |    ${(pageSizeMappingsST, ",\n")}
              |}
              |
              |with open(f"{output_dir}/{sdf_path}", "w+") as f:
              |    f.write(add_page_size(sdf.render(), page_size_mappings))"""
        else
          st"""with open(f"{output_dir}/{sdf_path}", "w+") as f:
              |    f.write(sdf.render())"""

      val marker = Marker.createHashMarker("META MARKER")

      val tq = "\"\"\""
      val metaContent: ST =
        st"""# Copyright 2025, UNSW
            |# SPDX-License-Identifier: BSD-2-Clause
            |import argparse
            |import struct
            |${xmlImportOpt}from random import randint
            |from dataclasses import dataclass
            |from typing import List, Tuple, Optional
            |from sdfgen import SystemDescription, Sddf, DeviceTree, LionsOs
            |from importlib.metadata import version
            |
            |${CommentTemplate.safeToEditComment_hash}
            |
            |assert int(version('sdfgen').split(".")[1]) >= 30, f"Requires sdfgen >= 0.30, found {version('sdfgen')}"
            |
            |from sdfgen_helper import *
            |
            |ProtectionDomain = SystemDescription.ProtectionDomain
            |MemoryRegion = SystemDescription.MemoryRegion
            |Map = SystemDescription.Map
            |Channel = SystemDescription.Channel
            |IrqConventional = SystemDescription.IrqConventional
            |VirtualMachine = SystemDescription.VirtualMachine
            |
            |@dataclass
            |class Board:
            |    name: str
            |    arch: SystemDescription.Arch
            |    paddr_top: int
            |    serial: str
            |    timer: str
            |    ethernet: str
            |    i2c: Optional[str]
            |
            |
            |BOARDS: List[Board] = [
            |    Board(
            |        name="qemu_virt_aarch64",
            |        arch=SystemDescription.Arch.AARCH64,
            |        paddr_top=0x6_0000_000,
            |        serial="pl011@9000000",
            |        timer="timer",
            |        ethernet="virtio_mmio@a003e00",
            |        i2c=None,
            |    ),
            |]
            |
            |def schedule(*entries):
            |    $tq
            |    entries: sequence of (channel, timeslice_ns)
            |    $tq
            |    part_ch, part_timeslices, is_user_partition = zip(*entries)
            |    return UserSchedule(list(part_timeslices), list(part_ch), list(is_user_partition))
            |
            |def generate(sdf_path: str, output_dir: str, dtb: DeviceTree):
            |    timer_node = dtb.node(board.timer)
            |    assert timer_node is not None
            |
            |    timer_driver = ProtectionDomain("timer_driver", "timer_driver.elf", priority=201)
            |    timer_system = Sddf.Timer(sdf, timer_node, timer_driver)
            |
            |    scheduler = ProtectionDomain("scheduler", "scheduler.elf", priority=200)
            |
            |    ${(templateManagedMapsSt, "\n")}
            |
            |    ${marker.beginMarker}
            |
            |    #######################################
            |    # PARTITION PROTECTION DOMAINS
            |    #######################################
            |    ${(partitionProtectionDomainsMons, "\n")}
            |
            |    #######################################
            |    # MEMORY REGIONS
            |    #######################################
            |    ${(memoryRegionsST, "\n")}
            |
            |    ${(mapsSt, "\n")}
            |
            |    $vmSectionOpt
            |
            |    #######################################
            |    # CHANNELS
            |    #######################################
            |    ${(monChannels, "\n")}
            |
            |    ${(channelsST, "\n")}
            |
            |    #######################################
            |    # SCHEDULE
            |    #######################################
            |    ${(schedulePairs, "\n")}
            |
            |    user_schedule = schedule(
            |      ${(schedule, ",\n")}
            |    )
            |
            |    ${marker.endMarker}
            |
            |    sdf.add_pd(timer_driver)
            |    sdf.add_pd(scheduler)
            |    timer_system.add_client(scheduler)
            |
            |    assert timer_system.connect()
            |    assert timer_system.serialise_config(output_dir)
            |
            |    data_path = output_dir + "/schedule_config.data"
            |    with open(data_path, "wb+") as f:
            |        f.write(user_schedule.serialise())
            |    update_elf_section(obj_copy, scheduler.program_image,
            |                       user_schedule.section_name,
            |                       data_path)
            |
            |    $renderST
            |
            |
            |if __name__ == '__main__':
            |    parser = argparse.ArgumentParser()
            |    parser.add_argument("--dtb", required=True)
            |    parser.add_argument("--sddf", required=True)
            |    parser.add_argument("--board", required=True, choices=[b.name for b in BOARDS])
            |    parser.add_argument("--output", required=True)
            |    parser.add_argument("--sdf", required=True)
            |    parser.add_argument("--objcopy", required=True)
            |
            |    args = parser.parse_args()
            |
            |    # Import the config structs module from the build directory
            |    sys.path.append(args.output)
            |    from config_structs import *
            |
            |    board = next(filter(lambda b: b.name == args.board, BOARDS))
            |
            |    sdf = SystemDescription(board.arch, board.paddr_top)
            |    sddf = Sddf(args.sddf)
            |
            |    global obj_copy
            |    obj_copy = args.objcopy
            |
            |    with open(args.dtb, "rb") as f:
            |        dtb = DeviceTree(f.read())
            |
            |    generate(args.sdf, args.output, dtb)
            |"""

      val sdScheduleXmlPath = s"${options.sel4OutputDir.get}/${msd.prefix}meta.py"
      resources = resources :+ ResourceUtil.createResourceWithMarkers(sdScheduleXmlPath, metaContent, ISZ(marker), F, F)
    }


    @pure def emitStaticSdFResources(): Unit = {
      val sdfgen_helperPath = s"${options.sel4OutputDir.get}/sdfgen_helper.py"
      resources = resources :+ ResourceUtil.createResourceH(
        path = sdfgen_helperPath, content = StaticContent.sdfen_helper_py,
        overwrite = T, isDatatype = F)

      val schedulerPath = s"${options.sel4OutputDir.get}/scheduler"

      val scheduler_config_h_Path = s"$schedulerPath/include/default.scheduler_config.h"
      resources = resources :+ ResourceUtil.createResourceH(
        path = scheduler_config_h_Path, content = StaticContent.scheduler_config_h,
        overwrite = T, isDatatype = F)

      val user_config_h_Path = s"$schedulerPath/include/default.user_config.h"
      resources = resources :+ ResourceUtil.createResourceH(
        path = user_config_h_Path, content = StaticContent.user_config_h,
        overwrite = T, isDatatype = F)

      val scheduler_c_Path = s"$schedulerPath/src/default.scheduler.c"
      resources = resources :+ ResourceUtil.createResourceH(
        path = scheduler_c_Path, content = StaticContent.scheduler_c,
        overwrite = T, isDatatype = F)
    }

    emitStaticSdFResources()

    return (localStore, resources)
  }

}

object StaticContent {

  val sdfen_helper_py: ST =
    st"""#!/usr/bin/env python3
        |# Copyright 2025, UNSW SPDX-License-Identifier: BSD-2-Clause
        |
        |import argparse
        |import sys
        |import re
        |import subprocess
        |import shutil
        |from os import path
        |
        |${CommentTemplate.doNotEditComment_hash}
        |
        |### Explanation
        |# Since this script generates the python classes that will be used by the metaprogram, it will
        |# typically be run before the build process and whenever a configuration struct is changed. It will
        |# generate a python struct class and serialisable class for each struct definition in each C header
        |# that it is passed. These classes make the process of creating a meta program much simpler and less
        |# error prone. Run the script on all the config headers used by your system (that are not serialised
        |# by the sdfgen tool) before creating your meta program and you can import the generated classes
        |# into your meta program and use them to create data files to be copied into your elfs.
        |
        |# This script also contains a couple of other useful functions for creating copies of elf files and
        |# updating elf sections with the data files serialised by the generated classes.
        |
        |### Assumptions
        |# If macros or types are found in config files that are not defined, the script will output them
        |# upon termination and will not create the python classes. If the unknown type is a c type, simply
        |# add it to the c_type_to_p_class dictionary. If macros are unknown, their values can be passed to
        |# the script as arguments. If a C type is unknown and defined in another header, either include this
        |# header in the files passed to the script, or temporarily add the definition to one of the config
        |# files passed to generate the the python classes, then remove before building your system.
        |#
        |# The script assumes that config headers are passed in order of dependencies, so be sure to pass
        |# files containing definitions that are used in other files first.
        |#
        |# The script assumes that all comments are across entire lines.
        |#
        |# Currently the script only supports simple single value macro substitutions and will fail to
        |# recognise more complex expressions.
        |#
        |# This script assumes that a struct field with the prefix "num_" refers to the length of another
        |# field if the substring following "num_" is equal to the name of the other field. If this is true,
        |# the field prefixed with "num_" will not be treated as a separate field to the matching field, and
        |# instead will be set to the length of the matching field.
        |
        |# Creates a new elf with elf_number as prefix. Adds ".elf" to elf strings
        |def copy_elf(source_elf: str, new_elf: str, elf_number = None):
        |    source_elf += ".elf"
        |    if elf_number != None:
        |        new_elf += str(elf_number)
        |    new_elf += ".elf"
        |    assert path.isfile(source_elf)
        |    return shutil.copyfile(source_elf, new_elf)
        |
        |# Copiers data region data_name into section_name of elf_name
        |def update_elf_section(obj_copy: str, elf_name: str, section_name: str, data_name: str):
        |    assert path.isfile(elf_name)
        |    assert path.isfile(data_name)
        |    assert subprocess.run([obj_copy, "--update-section", "." + section_name + "=" + data_name, elf_name]).returncode == 0
        |
        |c_name_regex = r"[a-zA-Z_][a-zA-Z0-9_]{0,63}"
        |# Currently we only support digits and macros for array sizes
        |c_value_regex = c_name_regex + r"|0b[01]+|0x[a-fA-F0-9]+|[0-9]+"
        |c_type_to_p_class = {
        |    "bool": "c_bool",
        |    "char": "c_char",
        |    "unsigned char": "c_ubyte",
        |    "short": "c_short",
        |    "unsigned short": "c_ushort",
        |    "int": "c_int",
        |    "unsigned int": "c_uint",
        |    "unsigned": "c_uint",
        |    "long": "c_long",
        |    "unsigned long": "c_ulong",
        |    "long long": "c_longlong",
        |    "float": "c_float",
        |    "double": "c_double",
        |    "long double": "c_longdouble",
        |    "uint8_t": "c_uint8",
        |    "uint16_t": "c_uint16",
        |    "uint32_t": "c_uint32",
        |    "uint64_t": "c_uint64",
        |    "size_t": "c_size_t",
        |    "ssize_t": "c_ssize_t",
        |    "uintptr_t": "c_uint64",
        |}
        |p_pointer_class = "c_uint64"
        |c_operators = ["+", "-", "*", "-"]
        |p_class_to_p_type = {
        |    "c_bool": "bool",
        |    "c_char": "str",
        |    "c_ubyte": "bytes",
        |    "c_short": "int",
        |    "c_ushort": "int",
        |    "c_int": "int",
        |    "c_uint": "int",
        |    "c_uint": "int",
        |    "c_long": "int",
        |    "c_ulong": "int",
        |    "c_longlong": "int",
        |    "c_uint8": "int",
        |    "c_uint16": "int",
        |    "c_uint32": "int",
        |    "c_uint64": "int",
        |    "c_size_t": "int",
        |    "c_ssize_t": "int",
        |    "c_float": "float",
        |    "c_double": "float",
        |    "c_longdouble": "float"
        |}
        |
        |# Macro and type classes
        |class Macro():
        |    # Store all known config macros
        |    all_macros = dict()
        |
        |    # Store all encountered undefined macros
        |    unknown_macros = dict()
        |
        |    def __init__(self, c_name, value):
        |        if c_name in Macro.all_macros:
        |            print(f"Duplicate definition found for macro {c_name}: {Macro.all_macros[c_name]}")
        |            sys.exit()
        |        self.c_name = c_name
        |        self.p_name = cNameToPName(c_name)
        |        self.value = value
        |        Macro.all_macros[c_name] = self
        |
        |class Struct():
        |    # Store all config structs
        |    all_structs = dict()
        |
        |    # Store all encountered undefined types
        |    unknown_types = dict()
        |
        |    # Input c_name without "_t" suffix
        |    def __init__(self, c_name):
        |        if c_name  + "_t" in Struct.all_structs:
        |            print(f"Duplicate definition found for type {c_name}: {Struct.all_structs[c_name]}")
        |            sys.exit()
        |        self.p_name = cNameToPName(c_name) + "Struct"
        |        self.c_name = c_name + "_t"
        |        self.fields = dict()
        |        Struct.all_structs[self.c_name] = self
        |
        |    def addField(self, field):
        |        if field.c_name in self.fields:
        |            print(f"Duplicate field {field.c_name} for type {self.c_name}")
        |            sys.exit()
        |        self.fields[field.c_name] = field
        |
        |class Field():
        |    def __init__(self, struct, c_name, c_type, c_size):
        |        self.c_name = c_name
        |        self.c_type = c_type
        |        self.c_size = c_size
        |
        |        # Extract python class and size and numeric size
        |        Field.cTypeToPClass(self, struct, c_type)
        |        Field.cSizeToPandNSize(self, struct, c_size)
        |
        |        # Add field to parent struct
        |        struct.addField(self)
        |
        |    def cTypeToPClass(self, struct, c_type):
        |        if c_type[-1] == "*":
        |            self.p_class = p_pointer_class
        |        elif c_type in c_type_to_p_class:
        |            self.p_class = c_type_to_p_class[c_type]
        |        elif c_type in Struct.all_structs:
        |            self.p_class = Struct.all_structs[c_type].p_name
        |        elif c_type not in Struct.unknown_types:
        |            Struct.unknown_types[c_type] = [(struct, self)]
        |            self.p_class = None
        |        else:
        |            Struct.unknown_types[c_type].append((struct, self))
        |            self.p_class = None
        |
        |    def cSizeToPandNSize(self, struct, c_size):
        |        self.p_size = []
        |        self.n_size = []
        |        eval_size = True
        |        for word in c_size:
        |            # Word is a digit or an operator
        |            if re.match(r"^[0-9]+$$", word) or word in c_operators:
        |                self.p_size.append(word)
        |                self.n_size.append(word)
        |            # Word is a macro
        |            elif word in Macro.all_macros:
        |                self.p_size.append(Macro.all_macros[word].p_name)
        |                self.n_size.append(Macro.all_macros[word].value)
        |            elif word not in Macro.unknown_macros:
        |                Macro.unknown_macros[word] = [(struct, self)]
        |                self.p_size.append(f"unknown")
        |                self.n_size.append(f"unknown")
        |                eval_size = False
        |            else:
        |                Macro.unknown_macros[word].append((struct, self))
        |                self.p_size.append(f"unknown")
        |                self.n_size.append(f"unknown")
        |                eval_size = False
        |        if len(c_size) and eval_size:
        |            self.e_size = eval("".join(self.n_size))
        |        else:
        |            self.e_size = ""
        |        return
        |
        |def cNameToPName(c_name):
        |    p_name = ""
        |    for word in c_name.lower().split("_"):
        |        p_name += word.capitalize()
        |
        |    return p_name
        |
        |if __name__ == '__main__':
        |    parser = argparse.ArgumentParser()
        |
        |    # Accept values for unknown Macros
        |    parser.add_argument("--output", required=True)
        |    parser.add_argument("--configs", required=True)
        |    parser.add_argument("--macros", required=False)
        |    args = parser.parse_args()
        |
        |    p_classes_out = args.output
        |
        |    # Store argument passed macros
        |    if args.macros:
        |        for macro_def in args.macros.split(" "):
        |            macro_val = macro_def.split("=")
        |            Macro(macro_val[0], macro_val[1])
        |
        |    # Extract struct files
        |    c_headers = args.configs.split(" ")
        |
        |    # Process all config headers
        |    for file in c_headers:
        |        with open(file, "r") as input:
        |            for line in input:
        |
        |                # Match on macros
        |                match = re.match(r"#define[ \t]+(" + c_name_regex + ")[ \t]+(" + c_value_regex + r")", line)
        |                if match:
        |                    c_name = match.group(1)
        |                    value = match.group(2)
        |                    macro = Macro(c_name, value)
        |                    continue;
        |
        |                # Match on struct typedef
        |                match = re.match(r"typedef[ \t]+struct[ \t]+(" + c_name_regex + ")[ \t]*{", line)
        |                if not match:
        |                    continue
        |                c_name = match.group(1)
        |                struct = Struct(c_name)
        |
        |                # Find struct fields
        |                line = next(input)
        |                # Line either does not have struct end } or contains definition
        |                while not re.match(r"[^;]*}.*", line):
        |
        |                    # Ignore comments and lines without definitions
        |                    if re.match(r"[ \t]*[/*]|^[^;]*$$", line):
        |                        line = next(input)
        |                        continue
        |
        |                    # Match on struct fields
        |                    match = re.match(r"[ \t]*(" + c_name_regex + r")(?:[ \t]+|[ \t]*([*]+)[ \t]*)(" + c_name_regex + r")[ \t]*(\[([^\]]*)?])?[ \t]*;", line)
        |                    c_name = match.group(3)
        |                    c_type = match.group(1)
        |                    c_size_literal = match.group(5)
        |
        |                    # Pointer type or field is array with empty size
        |                    if match.group(2) or (match.group(4) and not c_size_literal):
        |                        c_type += match.group(2)
        |
        |                    # Process array size
        |                    if c_size_literal:
        |                        c_size = [c_size_literal]
        |                    else:
        |                        c_size = []
        |
        |                    # Extract operators
        |                    for op in c_operators:
        |                        next_c_size = []
        |                        for word in c_size:
        |
        |                            # Ignore single characters of operators
        |                            if len(word) == 1:
        |                                next_c_size.append(word)
        |                                continue
        |
        |                            # Split string on each operator
        |                            args = word.split(op)
        |                            if len(args) == 1:
        |                                next_c_size.append(word)
        |                            else:
        |                                for arg in args[:-1]:
        |                                    next_c_size.append(arg.strip())
        |                                    next_c_size.append(op)
        |                                next_c_size.append(args[-1].strip())
        |
        |                        c_size = next_c_size
        |
        |                    field = Field(struct, c_name, c_type, c_size)
        |
        |                    # This was the last field
        |                    match = re.match(".*}.*", line)
        |                    if match:
        |                        break
        |                    else:
        |                        line = next(input)
        |
        |    with open(p_classes_out, "w") as out:
        |
        |        # Import modules
        |        out.write("from typing import List\n")
        |        out.write("from ctypes import *\n\n")
        |
        |        if Macro.unknown_macros or Struct.unknown_types:
        |            out.write("unknown = None\n\n")
        |
        |        if Macro.unknown_macros:
        |            out.write("# Missing macros used in struct fields:\n")
        |            for macro in Macro.unknown_macros.keys():
        |                out.write(f"# {macro}:\n")
        |                for struct, field in Macro.unknown_macros[macro]:
        |                    out.write(f"# - {struct.c_name}: {field.c_name}\n")
        |                out.write("\n")
        |            out.write("\n")
        |
        |        if Struct.unknown_types:
        |            out.write("# Missing types used in struct fields:\n")
        |            for struct in Struct.unknown_types.keys():
        |                out.write(f"# {struct}:\n")
        |                for missing_struct, field in Struct.unknown_types[struct]:
        |                    out.write(f"# - {missing_struct.c_name}: {field.c_name}\n")
        |                out.write("\n")
        |            out.write("\n")
        |
        |        if Macro.unknown_macros or Struct.unknown_types:
        |            sys.exit()
        |
        |        out.write("# Macros:\n")
        |        for macro in Macro.all_macros.values():
        |            out.write(f"# {macro.c_name}\n")
        |            out.write(f"{macro.p_name} = {macro.value}\n")
        |        out.write("\n")
        |
        |        # Create struct classes
        |        out.write("# Struct Classes\n")
        |        for struct in Struct.all_structs.values():
        |            out.write(f"# {struct.c_name}\nclass {struct.p_name}(LittleEndianStructure):\n")
        |            next_line = f"    _fields_ = ["
        |            for i, field in zip(range(len(struct.fields.values())), struct.fields.values()):
        |
        |                # Handle array fields
        |                next_line += f"(\"{field.c_name}\", {field.p_class}"
        |                comment_line = " " * 17 + f"# C type: {field.c_type}"
        |                if len(field.n_size) == 1:
        |                    next_line += f" * {field.n_size[0]}"
        |                    comment_line += f", C array size: {field.c_size[0]} ({field.p_size[0]})"
        |                # Multi argument size
        |                elif len(field.n_size) > 1:
        |                    next_line += " * ("
        |                    comment_line += ", C array size: "
        |                    size_count = 0
        |                    for raw, py, c in zip(field.n_size, field.p_size, field.c_size):
        |                        size_count += 1
        |                        plain_text = re.match(r"^[0-9]+$$", py) or py in c_operators
        |                        if size_count < len(field.n_size):
        |                            next_line += f"{raw} "
        |                            if plain_text:
        |                                comment_line += f"{c} "
        |                            else:
        |                                comment_line += f"{c} ({py}) "
        |                        else:
        |                            next_line += f"{raw})"
        |                            if plain_text:
        |                                comment_line += f"{c}"
        |                            else:
        |                                comment_line += f"{c} ({py})"
        |                if i < len(struct.fields) - 1:
        |                    next_line += f"),\n"
        |                else:
        |                    next_line += f")]\n\n"
        |                comment_line += "\n"
        |                out.write(comment_line)
        |                out.write(next_line)
        |                next_line =  " " * 17
        |
        |        out.write("\n")
        |
        |        # Create serializable structs
        |        out.write("class Serializable():\n    def serialise(self):\n        return bytes(self.to_struct())\n\n")
        |        for struct in Struct.all_structs.values():
        |
        |            # Create arguments
        |            out.write(f"class {struct.p_name[:-6]}(Serializable):\n    def __init__(self")
        |            for field in struct.fields.values():
        |                if field.c_name[:4] == "num_" and field.c_name[4:] in struct.fields:
        |                    continue
        |                list_start = ""
        |                list_end = ""
        |                if len(field.n_size):
        |                    list_start = "List["
        |                    list_end = "]"
        |                if field.c_type in Struct.all_structs:
        |                    out.write(f", {field.c_name}: {list_start}{field.p_class[:-6]}{list_end}")
        |                else:
        |                    out.write(f", {field.c_name}: {list_start}{p_class_to_p_type[field.p_class]}{list_end}")
        |            out.write("):\n")
        |
        |            # Initialise field objects
        |            for field in struct.fields.values():
        |                if field.c_name[:4] == "num_" and field.c_name[4:] in struct.fields:
        |                    continue
        |                out.write(" " * 8 + f"self.{field.c_name} = {field.c_name}\n")
        |            out.write(" " * 8 + f"self.section_name = \"{struct.c_name[:-2]}\"\n")
        |            out.write("\n")
        |
        |            # Define serializable class to struct function
        |            out.write(" " * 4 + f"def to_struct(self) -> {struct.p_name}:\n")
        |            for field in struct.fields.values():
        |                out.write(" " * 8)
        |                if len(field.n_size) and field.c_type not in Struct.all_structs:
        |                    out.write(f"{field.c_name}_arg = self.{field.c_name} + [{field.p_class}()] * ({field.e_size} - len(self.{field.c_name}))")
        |                elif len(field.n_size) and field.c_type in Struct.all_structs:
        |                    out.write(f"{field.c_name}_arg = [x.to_struct() for x in self.{field.c_name}] + (({field.e_size} - len(self.{field.c_name})) * [{field.p_class}()])")
        |                elif field.c_type in Struct.all_structs:
        |                    out.write(f"{field.c_name}_arg = {field.p_class}() if self.{field.c_name} is None else self.{field.c_name}.to_struct()")
        |                elif field.c_name[:4] == "num_" and field.c_name[4:] in struct.fields:
        |                    out.write(f"{field.c_name}_arg = len(self.{field.c_name[4:]})")
        |                else:
        |                    out.write(f"{field.c_name}_arg = {field.p_class}() if self.{field.c_name} is None else self.{field.c_name}")
        |                out.write("\n")
        |
        |            # Create to_struct.serialise arguments
        |            out.write(" " * 8 + f"return {struct.p_name}(")
        |            for i, field in zip(range(len(struct.fields.values())), struct.fields.values()):
        |                if field.n_size:
        |                    out.write(f"({field.p_class} * {field.e_size})(*{field.c_name}_arg)")
        |                else:
        |                    out.write(f"{field.c_name}_arg")
        |                if i < len(struct.fields) - 1:
        |                    out.write(", \n" + " " * (16 + len(struct.p_name)))
        |            out.write(")\n\n")
        |"""

  val scheduler_c: ST =
    st"""#include <stdint.h>
        |#include <stdbool.h>
        |
        |#include <microkit.h>
        |#include <sel4/sel4.h>
        |#include <os/sddf.h>
        |#include <sddf/timer/client.h>
        |#include <sddf/timer/config.h>
        |#include <sddf/util/printf.h>
        |
        |#include <default.scheduler_config.h>
        |#include <default.user_config.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |/* Number of nanoseconds in a second */
        |#define NS_IN_S  1000000000ULL
        |
        |__attribute__((__section__(".timer_client_config"))) timer_client_config_t config;
        |__attribute__((__section__(".user_schedule"))) user_schedule_t user_schedule;
        |
        |uint32_t current_timeslice;
        |
        |// Bitstring for partition ready status. 0 = not ready, 1 = ready.
        |uint64_t part_ready;
        |uint64_t part_ready_check;
        |
        |bool scheduler_running;
        |
        |void notify() {
        |    microkit_channel ch = user_schedule.timeslice_ch[current_timeslice];
        |    if (ch != 0) { // channel 0 is used to pad out a schedule
        |        microkit_notify(ch);
        |    }
        |    // Set a timeout for the length of this partition's timeslice
        |    sddf_timer_set_timeout(config.driver_id, user_schedule.timeslices[current_timeslice]);
        |}
        |
        |void next_partition() {
        |    current_timeslice = (current_timeslice + 1) % user_schedule.num_timeslices;
        |    notify();
        |}
        |
        |void notified(microkit_channel ch)
        |{
        |    if (ch == config.driver_id) {
        |        // Timer driver fired — either the initial timeout after all partitions reported
        |        // ready (starts the schedule) or a periodic tick (advances to the next timeslice).
        |        if (scheduler_running == false) {
        |            notify();
        |            scheduler_running = true;
        |        } else {
        |            next_partition();
        |        }
        |    } else if ((part_ready_check & (1 << ch)) != 0) { // valid channel belonging to  partition
        |        if ((part_ready & (1 << ch)) == 0) {
        |            // Channel not yet marked ready — this is the partition's first notification
        |            // during the initialisation handshake (before the schedule starts running).
        |
        |            sddf_dprintf("SCHEDULER | Marking partition %d as ready\n", ch);
        |            part_ready |= (1 << ch);  // Set this channel's bit in the readiness bitstring
        |
        |            if (part_ready == part_ready_check) {  // All expected partitions have reported ready
        |                sddf_dprintf("SCHEDULER | All partitions ready, beginning schedule\n");
        |                // Timeout to let the last partition become passive before starting
        |                sddf_timer_set_timeout(config.driver_id, NS_IN_S);
        |            }
        |        } else {
        |            // Runtime signals from partitions are ignored: the schedule is static and each
        |            // partition runs for its full allotted time regardless of early completion.
        |        }
        |    } else {
        |        sddf_dprintf("SCHEDULER |received unknown notification on channel: %d\n", ch);
        |    }
        |}
        |
        |void init(void)
        |{
        |    current_timeslice = 0;
        |
        |    scheduler_running = false;
        |
        |    part_ready |= (1 << 0); // ch 0 is always 'ready'
        |    part_ready_check |= (1 << 0); // ch 0 is always 'ready' -- keeps padding optional
        |
        |    // Build a bitmask of all channels that must report ready before the schedule
        |    // can start. Each bit position corresponds to a channel ID. During the
        |    // initialisation handshake, partitions notify on their channel; part_ready
        |    // accumulates those bits and is compared against this mask to detect when
        |    // every scheduled partition has checked in.
        |    for (int i = 0; i < user_schedule.num_timeslices; i++) {
        |        part_ready_check |= (1 << user_schedule.timeslice_ch[i]);
        |    }
        |}
        |"""

  val user_config_h: ST =
    st"""#pragma once
        |
        |#include <stdbool.h>
        |#include <stdint.h>
        |#include <default.scheduler_config.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// The metaprogram will omit a binary with the same format as this struct,
        |// and will be patched into the scheduler at system build time
        |
        |typedef struct user_schedule {
        |    uint64_t timeslices[MAX_SCHEDULE_SLOTS];
        |    // @kwinter: In the future, will also need to keep track of
        |    // all the PD's in a partition so that we can easily suspend them
        |    uint32_t timeslice_ch[MAX_SCHEDULE_SLOTS];
        |    bool is_user_partition[MAX_SCHEDULE_SLOTS];
        |    uint32_t num_timeslices;
        |} user_schedule_t;"""

  val scheduler_config_h: ST =
    st"""#pragma once
        |
        |#include <microkit.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// The max partitions is limited by the number of channels that we can establish
        |// between the scheduler and a partition's initial process in microkit.
        |// One channel is taken by the sDDF timer subsystem.
        |#define MAX_PARTITIONS (MICROKIT_MAX_CHANNELS - 1)
        |
        |// Maximum number of timeslice slots in a schedule.  A thread may appear
        |// in multiple slots per frame period, so this can exceed MAX_PARTITIONS.
        |// Must fit within the 4 KB shared-memory page (struct ≈ 13*N + 4 bytes).
        |#define MAX_SCHEDULE_SLOTS 128
        |
        |typedef struct schedule_config {
        |    uint32_t num_partitions;
        |    // Currently this is in NS
        |    uint64_t timeslices[MAX_PARTITIONS];
        |    microkit_channel partition_initial_pd[MAX_PARTITIONS];
        |} schedule_config_t;
        |"""

}