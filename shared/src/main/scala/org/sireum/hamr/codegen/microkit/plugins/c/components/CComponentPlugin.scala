// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.c.components

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, IdPath, MapValue, Store}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.properties.{Hamr_Microkit_Properties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlSystem, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.plugins.MicrokitPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.connections.CConnectionProviderPlugin
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.Aadl
import org.sireum.hamr.ir
import org.sireum.message.Reporter

object CComponentPlugin {
  val name: String = "CComponentPlugin"

  val KEY_CComponentR2U2Contributions: String = "KEY_CComponentR2U2Contributions"

  @strictpure def getCComponentR2U2Contributions(store: Store): Map[IdPath, CComponentR2U2Contributions] =
    store.getOrElse(KEY_CComponentR2U2Contributions,
      MapValue[IdPath, CComponentR2U2Contributions](Map.empty[IdPath, CComponentR2U2Contributions])).
      asInstanceOf[MapValue[IdPath, CComponentR2U2Contributions]].map

  @strictpure def getCComponentR2U2ContributionsFor(path: IdPath, store: Store): CComponentR2U2Contributions =
    getCComponentR2U2Contributions(store).get(path).getOrElse(CComponentR2U2Contributions.empty)

  @strictpure def putCComponentR2U2Contributions(path: IdPath, contributions: CComponentR2U2Contributions, store: Store): Store =
    store + KEY_CComponentR2U2Contributions ~> MapValue(getCComponentR2U2Contributions(store) + path ~> contributions)

  val CComponentPlugins: ISZ[Plugin] = ISZ(
    CComponentPlugin_DomainScheduler(),
    CComponentPlugin_MCS()
  )

  @strictpure def processedThreads(store: Store): B = store.contains(name)

  // Generate the native C R2U2 files from monitor-specific contributions.
  @pure def r2u2Resources(component: AadlThread,
                          options: HamrCli.CodegenOption,
                          contributions: CComponentR2U2Contributions): ISZ[Resource] = {
    val componentDir: String =
      s"${options.sel4OutputDir.get}/${MicrokitCodegen.dirComponents}/${MicrokitUtil.getComponentIdPath(component)}"
    if (!contributions.requiresR2U2) {
      return ISZ(
        ResourceUtil.createRemoveResource(
          s"$componentDir/${MicrokitCodegen.dirInclude}/r2u2_monitor.h", CommentTemplate.doNotEditComment),
        ResourceUtil.createRemoveResource(
          s"$componentDir/${MicrokitCodegen.dirSrc}/r2u2_monitor.c", CommentTemplate.doNotEditComment),
        ResourceUtil.createRemoveResource(
          s"$componentDir/${MicrokitCodegen.dirSrc}/spec.c2po", CommentTemplate.doNotEditComment),
        ResourceUtil.createRemoveResource(
          s"$componentDir/${MicrokitCodegen.dirSrc}/spec.map", CommentTemplate.doNotEditComment))
    }

    val specs: RAST.R2U2SpecDef = contributions.r2u2SpecDef.get
    val specCount: Z = specs.ftspecs.size + specs.ptspecs.size
    val headerItems: ISZ[ST] = contributions.r2u2HeaderItems :+
      st"""void r2u2_monitor_initialize(void);
          |void r2u2_monitor_pre_timeTriggered(void);
          |void r2u2_monitor_post_timeTriggered(void);"""
    val header: ST =
      st"""#pragma once
          |
          |#include <sb_types.h>
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${(headerItems, "\n\n")}
          |"""

    val monitorItems: ISZ[ST] = contributions.r2u2MonitorItems :+
      st"""// Cache the newest verdict returned for each specification.
          |static r2u2_status_t r2u2_cache_output(
          |    r2u2_mltl_instruction_t instruction,
          |    r2u2_verdict *verdict) {
          |  size_t spec_number = instruction.op2_value;
          |  if (verdict == NULL || spec_number >= R2U2_SPEC_COUNT) {
          |    return R2U2_ERR_OTHER;
          |  }
          |  r2u2_monitor.verdict_cache[spec_number] = *verdict;
          |  r2u2_monitor.verdict_valid[spec_number] = true;
          |  r2u2_monitor.verdict_updated[spec_number] = true;
          |  return R2U2_OK;
          |}"""
    val postItems: ISZ[ST] =
      (contributions.r2u2PostItems :+
        st"""for (size_t i = 0; i < R2U2_SPEC_COUNT; ++i) {
            |  r2u2_monitor.verdict_updated[i] = false;
            |}
            |r2u2_status_t status = r2u2_step(&r2u2_monitor.monitor);
            |if (status != R2U2_OK) {
            |  printf("R2U2 monitor step failed: %d\n", (int) status);
            |  return;
            |}""") ++ contributions.r2u2OutputItems
    val source: ST =
      st"""#include "${MicrokitUtil.getComponentIdPath(component)}.h"
          |#include <r2u2.h>
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |extern unsigned char r2u2_spec_bin[];
          |extern unsigned int r2u2_spec_bin_len;
          |
          |#define R2U2_SPEC_COUNT ${specCount}U
          |
          |typedef struct {
          |  r2u2_monitor_t monitor;
          |  r2u2_verdict verdict_cache[R2U2_SPEC_COUNT]; // Cache latest verdict (if applicable) per C2PO specification between monitor steps.
          |  bool verdict_valid[R2U2_SPEC_COUNT]; // Track whether each cached verdict is current.
          |  bool verdict_updated[R2U2_SPEC_COUNT]; // Track whether each verdict was updated during this monitor step.
          |} r2u2_monitor_state_t;
          |
          |// Instance of the R2U2 monitor.
          |static r2u2_monitor_state_t r2u2_monitor = {
          |  .monitor = R2U2_DEFAULT_MONITOR
          |};
          |
          |${(monitorItems, "\n\n")}
          |
          |void r2u2_monitor_initialize(void) {
          |  for (size_t i = 0; i < R2U2_SPEC_COUNT; ++i) {
          |    r2u2_monitor.verdict_valid[i] = false;
          |  }
          |  r2u2_monitor.monitor.out_file = NULL;
          |  r2u2_monitor.monitor.out_func = r2u2_cache_output;
          |  if (r2u2_spec_bin_len == 0U) {
          |    printf("R2U2 specification is empty\n");
          |    return;
          |  }
          |  r2u2_status_t status = r2u2_load_specification(r2u2_spec_bin, &r2u2_monitor.monitor);
          |  if (status != R2U2_OK) {
          |    printf("R2U2 specification load failed: %d\n", (int) status);
          |    return;
          |  }
          |}
          |
          |void r2u2_monitor_pre_timeTriggered(void) {
          |  ${(contributions.r2u2PreItems, "\n\n")}
          |}
          |
          |void r2u2_monitor_post_timeTriggered(void) {
          |  ${(postItems, "\n\n")}
          |}
          |"""

    return ISZ(
      ResourceUtil.createResource(
        s"$componentDir/${MicrokitCodegen.dirInclude}/r2u2_monitor.h", header, T),
      ResourceUtil.createResource(
        s"$componentDir/${MicrokitCodegen.dirSrc}/r2u2_monitor.c", source, T),
      ResourceUtil.createResource(s"$componentDir/${MicrokitCodegen.dirSrc}/spec.c2po",
        st"""${CommentTemplate.doNotEditComment_c2po}
            |${specs.prettyST}""", T),
      ResourceUtil.createResource(s"$componentDir/${MicrokitCodegen.dirSrc}/spec.map",
        st"""${CommentTemplate.doNotEditComment_c2po}
            |${specs.printMap}""", T))
  }

}

// Contributions used to add an R2U2 monitor to a native C component.
@datatype class CComponentR2U2Contributions(val requiresR2U2: B,
                                            val r2u2SpecDef: Option[RAST.R2U2SpecDef],
                                            val r2u2HeaderItems: ISZ[ST],
                                            val r2u2MonitorItems: ISZ[ST],
                                            val r2u2PreItems: ISZ[ST],
                                            val r2u2PostItems: ISZ[ST],
                                            val r2u2OutputItems: ISZ[ST]) {
  @strictpure def headerIncludes: ISZ[ST] =
    if (requiresR2U2) ISZ(st"#include \"r2u2_monitor.h\"") else ISZ()

  @strictpure def initializePost: ISZ[ST] =
    if (requiresR2U2) ISZ(st"r2u2_monitor_initialize();") else ISZ()

  @strictpure def computePre: ISZ[ST] =
    if (requiresR2U2) ISZ(st"r2u2_monitor_pre_timeTriggered();") else ISZ()

  @strictpure def computePost: ISZ[ST] =
    if (requiresR2U2) ISZ(st"r2u2_monitor_post_timeTriggered();") else ISZ()
}

object CComponentR2U2Contributions {
  @strictpure def empty: CComponentR2U2Contributions =
    CComponentR2U2Contributions(F, None(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ())
}

@sig trait CComponentPlugin extends MicrokitPlugin {

  @strictpure def hasHandled(store: Store): B = CComponentPlugin.processedThreads(store)

  @strictpure def markAsHandled(store: Store): Store = store + CComponentPlugin.name ~> BoolValue(T)

  @strictpure def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CConnectionProviderPlugin.getCConnectionStoreOpt(store).nonEmpty &&
      !hasHandled(store)
}