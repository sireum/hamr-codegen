// #Sireum
/*
Copyright (c) 2026, Collins Aerospace.
Developed with the sponsorship of Defense Advanced Research Projects Agency (DARPA).

Permission is hereby granted, free of charge, to any person obtaining a copy of this data,
including any software or models in source or binary form, as well as any drawings, specifications,
and documentation (collectively "the Data"), to deal in the Data without restriction, including
without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Data, and to permit persons to whom the Data is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Data.

THE DATA IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS, SPONSORS, DEVELOPERS, CONTRIBUTORS, OR COPYRIGHT HOLDERS BE LIABLE
FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE DATA OR THE USE OR OTHER DEALINGS IN THE DATA.
*/
package org.sireum.hamr.codegen.microkit.plugins.gumbo

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.symbols.{AadlComponent, AadlDataPort, AadlEventDataPort, AadlEventPort, AadlPort, AadlThread}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.plugins.gumbo.SlangExpUtil.{Context, TargetLanguage}
import org.sireum.hamr.codegen.microkit.plugins.rust.apis.{CRustApiUtil, ComponentApiContributions}
import org.sireum.hamr.codegen.microkit.plugins.rust.types.CRustTypeProvider
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.{GclAlert, GclStateVar}
import org.sireum.lang.{ast => SAST}
import org.sireum.message.Reporter

object GumboR2U2Util {

  @datatype class R2U2MonitorInput(val exp: RAST.Expr,
                                   val expType: GumboC2POUtil.C2POType.Type,
                                   val enumTypeOpt: Option[GumboC2POUtil.C2POEnum],
                                   val arrayTypeOpt: Option[GumboC2POUtil.C2POArray],
                                   val structTypeOpt: Option[GumboC2POUtil.C2POStruct],
                                   val referencedPorts: Set[String],
                                   val isPostStateVar: B)

  // Resolve the shared C2PO type information for a C or Rust monitor input.
  @pure def createMonitorInput(exp: RAST.Expr,
                               valueExp: SAST.Exp,
                               referencedPorts: Set[String],
                               isPostStateVar: B,
                               types: AadlTypes,
                               store: Store): R2U2MonitorInput = {
    val expType: GumboC2POUtil.C2POType.Type = GumboC2POUtil.getExprType(valueExp)
    val enumTypeOpt: Option[GumboC2POUtil.C2POEnum] =
      if (expType == GumboC2POUtil.C2POType.enumeration) Some(GumboC2POUtil.getEnumType(valueExp, types))
      else None()
    val arrayTypeOpt: Option[GumboC2POUtil.C2POArray] =
      if (expType == GumboC2POUtil.C2POType.array) GumboC2POUtil.getArrayType(valueExp, types, store)
      else None()
    val structTypeOpt: Option[GumboC2POUtil.C2POStruct] =
      if (expType == GumboC2POUtil.C2POType.struct) Some(GumboC2POUtil.getStructType(valueExp, types))
      else None()
    return R2U2MonitorInput(exp, expType, enumTypeOpt, arrayTypeOpt, structTypeOpt, referencedPorts, isPostStateVar)
  }

  // Add each struct's C2PO declarations and DEFINE, then replace the struct input
  // with one R2U2 input per field while preserving its port and dispatch metadata.
  @pure def expandStructInputs(specs: RAST.R2U2SpecDef,
                               monitorInputs: Map[String, R2U2MonitorInput]): (RAST.R2U2SpecDef, Map[String, R2U2MonitorInput]) = {
    var updatedSpecs: RAST.R2U2SpecDef = specs
    var expandedInputs: Map[String, R2U2MonitorInput] = Map.empty
    for (entry <- monitorInputs.entries) {
      val name: String = entry._1
      val input: R2U2MonitorInput = entry._2
      input.structTypeOpt match {
        case Some(structType) =>
          updatedSpecs = GumboC2POUtil.addStructDefinition(updatedSpecs, name, structType)
          for (field <- structType.fields) {
            val fieldName: String = s"${name}_${field.name}"
            expandedInputs = expandedInputs + fieldName ~> R2U2MonitorInput(
              exp = RAST.ExprST(st"(${input.exp.prettyST}).${field.name}"),
              expType = field.fieldType,
              enumTypeOpt = field.enumTypeOpt,
              arrayTypeOpt = field.arrayTypeOpt,
              structTypeOpt = None(),
              referencedPorts = input.referencedPorts,
              isPostStateVar = input.isPostStateVar)
          }
        case _ => expandedInputs = expandedInputs + entry
      }
    }
    return (updatedSpecs, expandedInputs)
  }

  // The C transport exposes peek for every port; only R2U2 components add it
  // to their generated Rust application API.
  @pure def peekApiContributions(thread: AadlThread,
                                 tp: CRustTypeProvider,
                                 store: Store): ComponentApiContributions = {
    var contributions = ComponentApiContributions.empty
    for (port <- thread.getPorts() if !StoreUtil.isSynthetic(port.path, store)) {
      contributions = contributions.combine(CRustApiUtil.processPeekPort(port, tp))
    }
    return contributions
  }

  // Route cached specification verdicts through the generated Rust monitor.
  @pure def processRustOutputs(thread: AadlThread,
                               orderedSpecs: ISZ[RAST.R2U2Formula],
                               alerts: ISZ[GclAlert]): (Option[RAST.Item], ISZ[RAST.BodyItem]) = {
    val alertSpecNumbers: Map[String, Z] = getAlertSpecNumbers(orderedSpecs, alerts)
    val alertedSpecNumbers: Set[Z] = Set.empty[Z] ++ alertSpecNumbers.values

    val loggedSpecs: ISZ[ST] = for (i <- 0 until orderedSpecs.size if !alertedSpecNumbers.contains(i)) yield
      st"($i, \"${orderedSpecs(i).id}\")"
    val loggedSpecsConstOpt: Option[RAST.Item] =
      if (loggedSpecs.nonEmpty) {
        Some(RAST.ItemST(
          st"""// Specifications without an alert mapping are logged after every monitor step.
              |const R2U2_LOGGED_SPECS: [(usize, &'static str); ${loggedSpecs.size}] = [
              |    ${(loggedSpecs, ",\n")}
              |];"""))
      } else {
        None()
      }

    var outputItems: ISZ[RAST.BodyItem] = ISZ(RAST.BodyItemST(
      st"""let r2u2_time_stamp = r2u2_monitor.monitor.time_stamp;
          |// Expire cached verdicts before applying this step's new outputs.
          |for verdict in r2u2_monitor.verdict_cache.iter_mut() {
          |    if verdict.is_some() && r2u2_time_stamp > verdict.unwrap().time {
          |        *verdict = None;
          |    }
          |}
          |// Cache the newest verdict returned for each specification.
          |let output_buffer = r2u2_core::get_output_buffer(&r2u2_monitor.monitor);
          |let verdict_cache = &mut r2u2_monitor.verdict_cache;
          |for out in output_buffer {
          |    verdict_cache[out.spec_num as usize] = Some(out.verdict);
          |}"""))

    if (loggedSpecs.nonEmpty) {
      outputItems = outputItems :+ RAST.BodyItemST(
        st"""// Report the current status of specifications without alert ports.
            |for (spec_num, spec_name) in R2U2_LOGGED_SPECS {
            |    let status = match r2u2_monitor.verdict_cache[spec_num] {
            |        Some(verdict) => if verdict.truth { "true" } else { "false" },
            |        None => "unknown",
            |    };
            |    log::info!("{} is currently {}", spec_name, status);
            |}""")
    }

    var alertOutputs: ISZ[ST] = ISZ()
    for (port <- thread.getPorts() if alertSpecNumbers.contains(port.identifier)) {
        val number = alertSpecNumbers.get(port.identifier).get
        val put: ST = port match {
          case _: AadlEventPort =>
            st"""if !verdict.truth {
                |    api.put_${port.identifier}();
                |}"""
          case _: AadlEventDataPort => st"api.put_${port.identifier}(verdict.truth);"
          case _ => halt("Unexpected R2U2 alert port type")
        }
        alertOutputs = alertOutputs :+ st"""if let Some(verdict) = r2u2_monitor.verdict_cache[$number] {
                                           |    $put
                                           |}"""
    }
    if (alertOutputs.nonEmpty) {
      outputItems = outputItems :+ RAST.BodyItemST(
        st"""// Send the latest cached verdict through each mapped alert port.
            |${(alertOutputs, "\n")}""")
    }
    return (loggedSpecsConstOpt, outputItems)
  }

  // Route cached specification verdicts through the generated C monitor.
  @pure def processCOutputs(thread: AadlThread,
                            orderedSpecs: ISZ[RAST.R2U2Formula],
                            alerts: ISZ[GclAlert]): (Option[ST], ISZ[ST]) = {
    val alertSpecNumbers: Map[String, Z] = getAlertSpecNumbers(orderedSpecs, alerts)
    val alertedSpecNumbers: Set[Z] = Set.empty[Z] ++ alertSpecNumbers.values
    val loggedSpecs: ISZ[ST] = for (i <- z"0" until orderedSpecs.size if !alertedSpecNumbers.contains(i)) yield
      st"{$i, \"${orderedSpecs(i).id}\"}"

    val loggedSpecDefinitions: Option[ST] =
      if (loggedSpecs.nonEmpty) {
        Some(st"""typedef struct {
                   |  size_t spec_number;
                   |  const char *spec_name;
                   |} r2u2_logged_spec_t;
                   |
                   |// Specifications without an alert mapping are logged after every monitor step.
                   |static const r2u2_logged_spec_t r2u2_logged_specs[${loggedSpecs.size}] = {
                   |  ${(loggedSpecs, ",\n")}
                   |};""")
      } else {
        None()
      }

    var outputItems: ISZ[ST] = ISZ(
      st"""// The callback runs during r2u2_step. Expire older cached verdicts
          |// against the new timestamp without discarding this step's outputs.
          |for (size_t i = 0; i < R2U2_SPEC_COUNT; ++i) {
          |  if (r2u2_monitor.verdict_valid[i] && !r2u2_monitor.verdict_updated[i] &&
          |      r2u2_monitor.monitor.time_stamp > get_verdict_time(r2u2_monitor.verdict_cache[i])) {
          |    r2u2_monitor.verdict_valid[i] = false;
          |  }
          |}""")

    if (loggedSpecs.nonEmpty) {
      outputItems = outputItems :+
        st"""// Report the current status of specifications without alert ports.
            |for (size_t i = 0; i < ${loggedSpecs.size}; ++i) {
            |  size_t spec_number = r2u2_logged_specs[i].spec_number;
            |  const char *status = "unknown";
            |  if (r2u2_monitor.verdict_valid[spec_number]) {
            |    status = get_verdict_truth(r2u2_monitor.verdict_cache[spec_number]) ? "true" : "false";
            |  }
            |  printf("%s is currently %s\n",
            |      r2u2_logged_specs[i].spec_name, status);
            |}"""
    }

    var alertOutputs: ISZ[ST] = ISZ()
    for (port <- thread.getPorts() if alertSpecNumbers.contains(port.identifier)) {
      val number: Z = alertSpecNumbers.get(port.identifier).get
      port match {
        case _: AadlEventPort =>
          alertOutputs = alertOutputs :+
            st"""if (r2u2_monitor.verdict_valid[$number] && !get_verdict_truth(r2u2_monitor.verdict_cache[$number])) {
                |  (void) put_${port.identifier}();
                |}"""
        case _: AadlEventDataPort =>
          alertOutputs = alertOutputs :+
            st"""if (r2u2_monitor.verdict_valid[$number]) {
                |  bool truth = get_verdict_truth(r2u2_monitor.verdict_cache[$number]);
                |  (void) put_${port.identifier}(&truth);
                |}"""
        case _ => halt("Unexpected C R2U2 alert port type")
      }
    }
    if (alertOutputs.nonEmpty) {
      outputItems = outputItems :+
        st"""// Send the latest cached verdict through each mapped alert port.
            |${(alertOutputs, "\n")}"""
    }
    return (loggedSpecDefinitions, outputItems)
  }

  // Map alert port identifiers to the specification ordering used by C2PO.
  @pure def getAlertSpecNumbers(orderedSpecs: ISZ[RAST.R2U2Formula],
                                alerts: ISZ[GclAlert]): Map[String, Z] = {
    val specNumbers: Map[String, Z] = Map.empty[String, Z] ++
      (for (i <- 0 until orderedSpecs.size) yield orderedSpecs(i).id ~> i)
    // GclResolver has already validated each guarantee-to-port mapping.
    return Map.empty[String, Z] ++
      (for (alert <- alerts) yield alert.portId ~> specNumbers.get(alert.guaranteeId).get)
  }

  // Lower a resolved Slang expression to an executable Rust R2U2 signal expression.
  // Port reads become dispatch-local observations. An absent event-data
  // payload uses its Rust default.
  @pure def lowerRustR2U2Input(exp: SAST.Exp,
                              component: AadlComponent,
                              context: Context.Type,
                              isAssumeRequires: B,
                              stateVars: ISZ[GclStateVar],
                              types: AadlTypes,
                              tp: CRustTypeProvider,
                              store: Store,
                              reporter: Reporter): R2U2MonitorInput = {
    // In(state) is sampled before dispatch; current state is sampled afterward.
    val stateVarOpt: Option[SAST.Exp.Ident] = getStateVar(exp, stateVars)
    val valueExp: SAST.Exp = stateVarOpt match {
      case Some(stateVar) => stateVar
      case _ => exp
    }
    // Rewrite ports and state variables to executable Rust forms, and collect
    // the ports observed in the pre- or post-dispatch hook.
    val expRewriter: R2U2ExpRewriter =
      R2U2ExpRewriter(TargetLanguage.rust, component, stateVars, types, store, reporter)
    val snapshotExp: SAST.Exp = expRewriter.transform_langastExp(exp) match {
      case MSome(e) => e
      case _ => exp
    }
    var rustExp = SlangExpUtil.rewriteExpH(
      rexp = snapshotExp,
      owner = component.classifier,
      optComponent = Some(component),
      context = context,
      inRequires = isAssumeRequires,
      inEnsures = F,
      target = TargetLanguage.rust,
      substitutions = Map.empty[String, String],
      aadlTypes = types,
      tp = tp,
      store = store,
      reporter = reporter)
    // Local subclause functions are emitted in the component's GUMBOX module.
    if (GumboC2POUtil.isGumboFunctionCall(valueExp, component.classifier)) {
      rustExp = st"GUMBOX::$rustExp"
    }
    snapshotExp.typedOpt match {
      // R2U2 loads a concrete payload value. Presence is loaded as a separate
      // Boolean signal (e.g., HasEvent(...)), so an absent optional payload can
      // safely use a default. The rewritten expression carries the executable
      // peek type rather than GCL's ghost-oriented port type.
      case Some(SAST.Typed.Name(SAST.Typed.optionName, _, _)) =>
        rustExp = st"${rustExp}.unwrap_or_default()"
      case _ =>
    }
    return createMonitorInput(
      exp = RAST.ExprST(rustExp),
      valueExp = valueExp,
      referencedPorts = expRewriter.referencedPorts,
      isPostStateVar = expRewriter.referencesPostStateVar,
      types = types,
      store = store)
  }

  // Lower a resolved Slang expression to a native C R2U2 monitor input.
  @pure def lowerCR2U2Input(exp: SAST.Exp,
                            component: AadlThread,
                            stateVars: ISZ[GclStateVar],
                            types: AadlTypes,
                            tp: CRustTypeProvider,
                            store: Store,
                            reporter: Reporter): (R2U2MonitorInput, Set[String]) = {
    val stateVarOpt: Option[SAST.Exp.Ident] = getStateVar(exp, stateVars)
    val valueExp: SAST.Exp = stateVarOpt match {
      case Some(stateVar) => stateVar
      case _ => exp
    }
    // Rewrite ports, state variables, and local function calls to their generated
    // C forms, and collect ports observed in the pre- or post-dispatch hook.
    val expRewriter: R2U2ExpRewriter =
      R2U2ExpRewriter(TargetLanguage.C, component, stateVars, types, store, reporter)
    val snapshotExp: SAST.Exp = expRewriter.transform_langastExp(exp) match {
      case MSome(e) => e
      case _ => exp
    }
    val cExp: ST = SlangExpUtil.rewriteExpH(
      rexp = snapshotExp,
      owner = component.classifier,
      optComponent = Some(component),
      context = Context.monitor_clause,
      substitutions = Map.empty[String, String],
      inRequires = F,
      inEnsures = F,
      target = TargetLanguage.C,
      tp = tp,
      aadlTypes = types,
      store = store,
      reporter = reporter)

    return (createMonitorInput(
      exp = RAST.ExprST(cExp),
      valueExp = valueExp,
      referencedPorts = expRewriter.referencedPorts,
      isPostStateVar = expRewriter.referencesPostStateVar,
      types = types,
      store = store), expRewriter.referencedMethods)
  }

  // Resolve a monitor input that refers to a local state variable.
  @pure def getStateVar(exp: SAST.Exp, stateVars: ISZ[GclStateVar]): Option[SAST.Exp.Ident] = {
    exp match {
      case SAST.Exp.Input(id: SAST.Exp.Ident) => return Some(id)
      case id: SAST.Exp.Ident if ops.ISZOps(stateVars).exists((stateVar: GclStateVar) => stateVar.name == id.id.value) =>
        return Some(id)
      case _ => return None()
    }
  }

  // Rewrite resolved GUMBO monitor inputs into executable C or Rust expressions,
  // collecting referenced ports, state timing, and C helper functions along the way.
  @record class R2U2ExpRewriter(val target: TargetLanguage.Type,
                                val component: AadlComponent,
                                val stateVars: ISZ[GclStateVar],
                                val types: AadlTypes,
                                val store: Store,
                                val reporter: Reporter) extends org.sireum.hamr.ir.MTransformer {
    val ports: Map[String, AadlPort] = Map.empty[String, AadlPort] ++
      component.getPorts().map((p: AadlPort) => p.identifier ~> p)
    val stateVarNames: Set[String] = Set.empty[String] ++
      stateVars.map((stateVar: GclStateVar) => stateVar.name)
    // Record information needed after expression rewriting.
    var referencedPorts: Set[String] = Set.empty
    var referencedMethods: Set[String] = Set.empty
    var referencesPreStateVar: B = F
    var referencesPostStateVar: B = F

    // Report an unsupported expression and continue with a placeholder.
    @pure def unsupported(exp: SAST.Exp, message: String): SAST.Exp = {
      reporter.error(exp.posOpt, MicrokitCodegen.toolName, message)
      return SAST.Exp.LitZ(0, SAST.Attr(exp.posOpt))
    }

    // Resolve a direct api.port selection to its AADL port.
    @pure def directPort(exp: SAST.Exp): Option[AadlPort] = {
      exp match {
        case SAST.Exp.Select(Some(SAST.Exp.Ident(SAST.Id("api"))), id, _) => return ports.get(id.value)
        case _ => return None()
      }
    }

    // Rename current state for the target and record its post-dispatch timing.
    override def pre_langastExpIdent(o: SAST.Exp.Ident): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.resOpt match {
        case Some(v: SAST.ResolvedInfo.Var)
          if v.isInObject && stateVarNames.contains(o.id.value) =>
          referencesPostStateVar = T
          if (target == TargetLanguage.C) {
            return org.sireum.hamr.ir.MTransformer.PreResult(
              F, MSome(o(id = o.id(value = s"r2u2_state_${o.id.value}"))))
          } else {
            return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(o(id = o.id(value = s"self.${o.id.value}"))))
          }
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(F, MNone[SAST.Exp]())
    }

    // Remove In(state), rename the state for the target, and record its pre-dispatch timing.
    override def pre_langastExpInput(o: SAST.Exp.Input): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      o.exp match {
        case id: SAST.Exp.Ident =>
          id.resOpt match {
            case Some(v: SAST.ResolvedInfo.Var)
              if v.isInObject && stateVarNames.contains(id.id.value) =>
              referencesPreStateVar = T
              val name: String = if (target == TargetLanguage.C) s"r2u2_state_${id.id.value}" else s"self.${id.id.value}"
              return org.sireum.hamr.ir.MTransformer.PreResult(
                F, MSome(id(id = id.id(value = name))))
            case _ =>
          }
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(
        F, MSome(unsupported(o, "R2U2 monitors only support In(stateVariable)")))
    }

    // Normalize C port operations and static array sizes before rewriting receivers.
    override def pre_langastExpSelect(o: SAST.Exp.Select): org.sireum.hamr.ir.MTransformer.PreResult[SAST.Exp] = {
      if (target == TargetLanguage.C) {
        o.receiverOpt match {
          case Some(receiver) =>
            o.id.value match {
              case "nonEmpty" =>
                directPort(receiver) match {
                  case Some(port) =>
                    referencedPorts = referencedPorts + port.identifier
                    return org.sireum.hamr.ir.MTransformer.PreResult(
                      F, MSome(SAST.Exp.Ident(SAST.Id(s"r2u2_port_${port.identifier}_present", o.id.attr), o.attr)))
                  case _ =>
                    return org.sireum.hamr.ir.MTransformer.PreResult(
                      F, MSome(unsupported(o, "C R2U2 monitors only support nonEmpty on event ports")))
                }
              case "isEmpty" =>
                directPort(receiver) match {
                  case Some(port) =>
                    referencedPorts = referencedPorts + port.identifier
                    val present: SAST.Exp = SAST.Exp.Ident(
                      SAST.Id(s"r2u2_port_${port.identifier}_present", o.id.attr), o.attr)
                    return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(SAST.Exp.Unary(
                      SAST.Exp.UnaryOp.Not, present, o.attr, o.id.attr.posOpt)))
                  case _ =>
                    return org.sireum.hamr.ir.MTransformer.PreResult(
                      F, MSome(unsupported(o, "C R2U2 monitors only support isEmpty on event ports")))
                }
              case "get" =>
                directPort(receiver) match {
                  case Some(port) =>
                    referencedPorts = referencedPorts + port.identifier
                    return org.sireum.hamr.ir.MTransformer.PreResult(
                      F, MSome(SAST.Exp.Ident(SAST.Id(s"r2u2_port_${port.identifier}", o.id.attr), o.attr)))
                  case _ =>
                }
              case "size" =>
                // GCL inserts Option.get between an event-data port and its array payload.
                val arrayExp: SAST.Exp = receiver match {
                  case select: SAST.Exp.Select =>
                    select.typedOpt match {
                      case Some(m: SAST.Typed.Method)
                        if m.owner == SAST.Typed.optionName && m.name == "get" => select.receiverOpt.get
                      case _ => receiver
                    }
                  case _ => receiver
                }
                GumboC2POUtil.getArrayType(arrayExp, types, store) match {
                  case Some(arrayType) =>
                    return org.sireum.hamr.ir.MTransformer.PreResult(
                      F, MSome(SAST.Exp.LitZ(arrayType.size, SAST.Attr(o.posOpt))))
                  case _ =>
                    return org.sireum.hamr.ir.MTransformer.PreResult(
                      F, MSome(unsupported(o, "Could not resolve the C R2U2 array size")))
                }
              case _ =>
            }
          case _ =>
        }
      }

      // Replace api.port with the target's dispatch-local snapshot expression.
      directPort(o) match {
        case Some(port) =>
          referencedPorts = referencedPorts + port.identifier
          val snapshot: SAST.Exp.Ident =
            if (target == TargetLanguage.C) {
              val name: String = port match {
                case _: AadlEventPort => s"r2u2_port_${port.identifier}_present"
                case _ => s"r2u2_port_${port.identifier}"
              }
              SAST.Exp.Ident(SAST.Id(name, o.id.attr), o.attr)
            } else {
              // Convert GCL's ghost port type to the executable Rust peek type.
              val snapshotTypedOpt: Option[SAST.Typed] = port match {
                case _: AadlEventPort =>
                  SAST.Typed.bOpt
                case _: AadlDataPort =>
                  o.typedOpt match {
                    case Some(SAST.Typed.Name(SAST.Typed.optionName, _, ISZ(payload))) => Some(payload)
                    case _ => o.typedOpt
                  }
                case _: AadlEventDataPort =>
                  o.typedOpt
              }
              SAST.Exp.Ident(id = o.id, attr = o.attr(typedOpt = snapshotTypedOpt))
            }
          return org.sireum.hamr.ir.MTransformer.PreResult(F, MSome(snapshot))
        case _ =>
      }
      return org.sireum.hamr.ir.MTransformer.PreResult(T, MNone[SAST.Exp]())
    }

    // Finish target-specific Option and event normalization after rewriting children.
    @pure override def post_langastExpSelect(o: SAST.Exp.Select): MOption[SAST.Exp] = {
      if (target == TargetLanguage.C) {
        // C snapshots contain concrete payloads, so remove the Option accessor.
        o.typedOpt match {
          case Some(m: SAST.Typed.Method)
            if m.owner == SAST.Typed.optionName && m.name == "get" && o.receiverOpt.nonEmpty =>
            return MSome(o.receiverOpt.get)
          case _ =>
        }
        return MNone()
      }

      o match {
        case SAST.Exp.Select(Some(snapshot@SAST.Exp.Ident(id)), operationId, _) =>
          ports.get(id.value) match {
            case Some(_: AadlEventDataPort) =>
              operationId.value match {
                // Event-data remains Option[payload]; SlangExpUtil will translate
                // nonEmpty/isEmpty to is_some()/is_none().
                case "nonEmpty" =>
                  return MNone()
                case "isEmpty" =>
                  return MNone()
                case "get" =>
                  // R2U2 loads presence separately, so use a default payload when
                  // the event-data port is empty instead of panicking on unwrap().
                  o.typedOpt match {
                    case Some(m: SAST.Typed.Method) if m.owner == SAST.Typed.optionName && m.name == "get" =>
                      return MSome(o(id = operationId(value = "unwrap_or_default()")))
                    case _ =>
                  }
                case _ =>
              }

            // A plain event getter returns B, so its snapshot already represents
            // presence: nonEmpty is the value and isEmpty is its negation.
            case Some(_: AadlEventPort) if operationId.value == "nonEmpty" || operationId.value == "isEmpty" =>
              if (operationId.value == "isEmpty") {
                return MSome(SAST.Exp.Unary(
                  op = SAST.Exp.UnaryOp.Not,
                  exp = snapshot,
                  attr = o.attr,
                  opPosOpt = operationId.attr.posOpt))
              } else {
                return MSome(snapshot)
              }
            case _ =>
          }
        case _ =>
      }
      return MNone()
    }

    // Rename C helper calls and normalize C array-port indexing.
    @pure override def post_langastExpInvoke(o: SAST.Exp.Invoke): MOption[SAST.Exp] = {
      if (target == TargetLanguage.C) {
        // Collect local helpers so GumboCPlugin emits each required function body.
        if (GumboC2POUtil.isGumboFunctionCall(o, component.classifier)) {
          referencedMethods = referencedMethods + o.ident.id.value
          return MSome(o(ident = o.ident(id = o.ident.id(value = s"r2u2_gumbo_${o.ident.id.value}"))))
        }
        // GCL represents array-port indexing as api.port(index).
        o.receiverOpt match {
          case Some(SAST.Exp.Ident(SAST.Id("api"))) =>
            ports.get(o.ident.id.value) match {
              case Some(port) =>
                referencedPorts = referencedPorts + port.identifier
                if (o.args.size == 1) {
                  return MSome(o(receiverOpt = None(),
                    ident = o.ident(id = o.ident.id(value = s"r2u2_port_${port.identifier}"))))
                } else {
                  return MSome(unsupported(o, "C R2U2 monitors only support one-dimensional array indexing"))
                }
              case _ =>
            }
          case _ =>
        }
      }
      return MNone()
    }
  }
}
