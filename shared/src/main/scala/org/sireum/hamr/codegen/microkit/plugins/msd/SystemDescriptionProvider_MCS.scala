// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.msd

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, Store}
import org.sireum.hamr.codegen.common.containers.{Marker, Resource}
import org.sireum.hamr.codegen.common.properties.Hamr_Microkit_Properties
import org.sireum.hamr.codegen.common.symbols.SymbolTable
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.plugins.c.components.CComponentPlugin
import org.sireum.hamr.codegen.microkit.util.SystemDescription
import org.sireum.hamr.ir.Aadl
import org.sireum.message.Reporter

object SystemDescriptionProvider_MCS {
  val metaPy: String = "meta.py"
}

@datatype class SystemDescriptionProvider_MCS extends SystemDescriptionProviderPlugin {

  val name: String = "SystemDescriptionProvider_MSC"

  @strictpure override def hasHandled(store: Store): B = store.contains(name)

  override def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B = {
    return super.canHandle(model, options, types, symbolTable, store, reporter) &&
      CComponentPlugin.getSchedulingType(symbolTable.rootSystem) == Hamr_Microkit_Properties.SchedulingType.MCS
  }

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    var localStore = store + name ~> BoolValue(T)
    var resources = ISZ[Resource]()

    val protectionDomains = StoreUtil.getProtectionDomains(localStore)
    val schedulingDomains = StoreUtil.getSchedulingDomains(localStore)
    val memoryRegions = StoreUtil.getMemoryRegions(localStore)
    val channesl = StoreUtil.getChannels(localStore)


    var partitionProtectionDomainsMons: ISZ[ST] = ISZ()
    var partitionProtectionDomainsUser: ISZ[ST] = ISZ()

    var initpds: ISZ[String] = ISZ()
    var userpds: ISZ[String] = ISZ()

    for (pd <- protectionDomains) {
      assert(pd.children.isEmpty, "Not expecting children protection domains for MCS")

      if (ops.StringOps(pd.name).endsWith("_MON")) {
        initpds = initpds :+ pd.name
        partitionProtectionDomainsMons = partitionProtectionDomainsMons :+
          st"""${pd.name} = ProctectionDomain("${pd.name}", "${pd.programImage}", priority=150, passive=True)"""
      } else {
        userpds = userpds :+ pd.name
        partitionProtectionDomainsUser = partitionProtectionDomainsUser :+
          st"""${pd.name} = ProctectionDomain("${pd.name}", "${pd.programImage}", priority=140, passive=True)"""
      }
    }

    var timeSlices: ISZ[Z] = ISZ()
    var channels: ISZ[Z] = ISZ()
    for (i <- 0 until schedulingDomains.size) {
      channels = channels :+ i
      timeSlices = timeSlices :+ schedulingDomains(i).length
    }

    val metaContent =
      st"""# Copyright 2025, UNSW
          |# SPDX-License-Identifier: BSD-2-Clause
          |import argparse
          |import struct
          |from random import randint
          |from dataclasses import dataclass
          |from typing import List, Tuple, Optional
          |from sdfgen import SystemDescription, Sddf, DeviceTree, LionsOs
          |from importlib.metadata import version
          |
          |assert version('sdfgen').split(".")[1] == "27", "Unexpected sdfgen version"
          |
          |from sdfgen_helper import *
          |
          |ProtectionDomain = SystemDescription.ProtectionDomain
          |MemoryRegion = SystemDescription.MemoryRegion
          |Map = SystemDescription.Map
          |Channel = SystemDescription.Channel
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
          |    ##############################
          |    # PARTITION PROTECTION DOMAINS
          |    ##############################
          |    ${(partitionProtectionDomainsMons, "\n")}
          |
          |    ${(partitionProtectionDomainsUser, "\n")}
          |
          |    partition_initial_pds = [
          |      ${(initpds, ",\n")}
          |    ]
          |
          |    partition_user_pds = [
          |      ${(userpds, ",\n")}
          |    ]
          |
          |    pds = [
          |      timer_driver,
          |      scheduler
          |    ]
          |
          |    for pd in pds:
          |      sdf.add_pd(pd)
          |
          |    for pd in partition_initial_pds:
          |      scheduler.add_child_pd(pd)
          |
          |
          |    #############
          |    # TIME SLICES
          |    #############
          |
          |    # These timeslices are in nanoseconds
          |    part_timeslices = [${(timeSlices, ", ")}]
          |
          |    part_ch = [${(channels, ", ")}]
          |
          |    # MEMORY REGIONS
          |
          |
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
          |    with open(f"{output_dir}/{sdf_path}", "w+") as f:
          |        f.write(sdf.render())
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


    val sdScheduleXmlPath = s"${options.sel4OutputDir.get}/${SystemDescriptionProvider_MCS.metaPy}"
    resources = resources :+ ResourceUtil.createResource(sdScheduleXmlPath, metaContent, T)

    return (localStore, resources)
  }
}