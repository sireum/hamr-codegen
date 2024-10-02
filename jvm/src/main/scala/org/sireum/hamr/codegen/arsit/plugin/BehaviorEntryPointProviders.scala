// #Sireum

package org.sireum.hamr.arsit.plugin

import org.sireum._
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin._
import org.sireum.hamr.arsit.templates.StubTemplate
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.CommonUtil.toolName
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, TypeUtil}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.ir.Direction
import org.sireum.message.Reporter

object BehaviorEntryPointProviders {

  @pure def getMarkers(entries: ISZ[BehaviorEntryPointObjectContributions]): ISZ[Marker] = {
    return entries.filter(f => f.isInstanceOf[BehaviorEntryPointMethodContributions])
      .map((m: BehaviorEntryPointObjectContributions) => m.asInstanceOf[BehaviorEntryPointMethodContributions]).flatMap(f => f.markers)
  }

  def offer(entryPoint: EntryPoints.Type,
            optInEventPort: Option[AadlPort],
            component: AadlThreadOrDevice,
            componentNames: NameProvider,
            excludeImpl: B,
            methodSig: String,
            defaultMethodBody: ST,
            annexClauseInfos: ISZ[AnnexClauseInfo],

            basePackageName: String,
            symbolTable: SymbolTable,
            aadlTypes: AadlTypes,
            projectDirs: ProjectDirectories,
            arsitOptions: ArsitOptions,

            plugins: MSZ[Plugin],
            reporter: Reporter): FullMethodContributions = {

    var ret = BehaviorEntryPointProviderPlugin.emptyFullContributions
    var optMethod: Option[ST] = None()
    var cases: ISZ[CaseContractBlock] = ISZ()
    var noncases: ISZ[NonCaseContractBlock] = ISZ()

    @strictpure def canHandle(p: Plugin): B =
      p.isInstanceOf[BehaviorEntryPointProviderPlugin] && p.asInstanceOf[BehaviorEntryPointProviderPlugin].canHandleBehaviorEntryPointProvider(entryPoint, optInEventPort, component, annexClauseInfos, arsitOptions, symbolTable, aadlTypes)

    var optBody: Option[ST] = None()
    for (p <- plugins if !reporter.hasError && canHandle(p)) {
      p.asInstanceOf[BehaviorEntryPointProviderPlugin].handleBehaviorEntryPointProvider(
        entryPoint, optInEventPort, component, componentNames, excludeImpl,
        methodSig, defaultMethodBody,
        annexClauseInfos,
        symbolTable, aadlTypes, projectDirs, arsitOptions, reporter) match {
        case b: FullMethodContributions =>
          if (optMethod.nonEmpty) {
            reporter.error(None(), toolName, "A behavior entry point plugin has already contributed a method implementation")
          } else if (optBody.nonEmpty) {
            reporter.error(None(), toolName, s"A behavior entry point plugin contributed a method body that cannot be combined with ${p.name}'s method")
          } else if (cases.nonEmpty || noncases.nonEmpty) {
            reporter.error(None(), toolName, s"A behavior entry point plugin contributed a contract block that cannot be combined with ${p.name}'s method")
          } else {
            optMethod = Some(b.method)
            ret = ret(
              tags = ret.tags ++ b.tags,
              imports = ret.imports ++ b.imports,
              preObjectBlocks = ret.preObjectBlocks ++ b.preObjectBlocks,
              preMethodBlocks = ret.preMethodBlocks ++ b.preMethodBlocks,
              postMethodBlocks = ret.postMethodBlocks ++ b.postMethodBlocks,
              postObjectBlocks = ret.postObjectBlocks ++ b.postObjectBlocks,
              markers = ret.markers ++ b.markers,
              resources = ret.resources ++ b.resources
            )
          }
        case b: PartialMethodContributions =>
          if (b.optBody.nonEmpty) {
            if (optBody.nonEmpty) {
              reporter.error(None(), toolName, s"A behavior entry point plugin contributed a method body that cannot be combined with ${p.name}'s body")
            }
            optBody = b.optBody
          }
          ret = ret(
            tags = ret.tags ++ b.tags,
            imports = ret.imports ++ b.imports,
            preObjectBlocks = ret.preObjectBlocks ++ b.preObjectBlocks,
            preMethodBlocks = ret.preMethodBlocks ++ b.preMethodBlocks,
            postMethodBlocks = ret.postMethodBlocks ++ b.postMethodBlocks,
            postObjectBlocks = ret.postObjectBlocks ++ b.postObjectBlocks,
            markers = ret.markers ++ b.markers,
            resources = ret.resources ++ b.resources
          )
          b.contractBlock match {
            case Some(i: CaseContractBlock) => cases = cases :+ i
            case Some(i: NonCaseContractBlock) => noncases = noncases :+ i
            case _ =>
          }
          if (cases.nonEmpty && noncases.nonEmpty) {
            reporter.error(None(), toolName, "Logika doesn't support a mix of contract cases and general contracts")
          }
      }
    }
    assert(optMethod.nonEmpty -->: (optBody.isEmpty && cases.isEmpty && noncases.isEmpty), "Sanity check: cannot combine methods with optional method bodies")

    if (optMethod.nonEmpty) {
      return ret(method = optMethod.get)
    } else {
      @pure def processNonCases(entries: ISZ[NonCaseContractBlock]): ST = {
        @strictpure def wrap(prefix: String, es: ISZ[ST]): Option[ST] =
          if (es.isEmpty) None()
          else Some(
            st"""$prefix(
                |  ${(es, ",\n")}
                |)""")

        val _entries = ISZ(
          wrap("Reads", entries.flatMap(f => f.contractReads)),
          wrap("Requires", entries.flatMap(f => f.contractRequires)),
          wrap("Modifies", entries.flatMap(f => f.contractModifies)),
          wrap("Ensures", entries.flatMap(f => f.contractEnsures)),
          wrap("InfoFlows", entries.flatMap(f => f.contractFlows))).filter(f => f.nonEmpty).map((m: Option[ST]) => m.get)

        return (st"""Contract(
                    |  ${(_entries, ",\n")}
                    |)""")
      }

      val optContract: Option[ST] =
        if (cases.nonEmpty) Some(
          st"""Contract(
              |  ${(cases, ",\n")}
              |)""")
        else if (noncases.nonEmpty) Some(processNonCases(noncases))
        else None()

      val body: ST = if (optBody.nonEmpty) optBody.get else defaultMethodBody

      val method: ST = if (optContract.isEmpty && body.render.size == 0) {
        st"$methodSig = { }"
      } else {
        st"""$methodSig = {
            |  $optContract
            |  $body
            |}"""
      }

      return ret(method = method)
    }
  }

  def finalise(annexClauseInfos: ISZ[AnnexClauseInfo],

               component: AadlThreadOrDevice,
               nameProvider: NameProvider,
               basePackageName: String,
               symbolTable: SymbolTable,
               aadlTypes: AadlTypes,
               projectDirs: ProjectDirectories,
               arsitOptions: ArsitOptions,

               plugins: MSZ[Plugin],
               reporter: Reporter): ObjectContributions = {

    var ret = BehaviorEntryPointProviderPlugin.emptyObjectContributions
    for (p <- plugins if !reporter.hasError && p.isInstanceOf[BehaviorEntryPointProviderPlugin] && p.asInstanceOf[BehaviorEntryPointProviderPlugin].canFinaliseBehaviorEntryPointProvider(component, annexClauseInfos, arsitOptions, symbolTable, aadlTypes)) {
      p.asInstanceOf[BehaviorEntryPointProviderPlugin].finaliseBehaviorEntryPointProvider(
        component, nameProvider, annexClauseInfos,
        symbolTable, aadlTypes, projectDirs, arsitOptions, reporter) match {
        case Some(x) =>
          ret = ret(
            tags = ret.tags ++ x.tags,
            imports = ret.imports ++ x.imports,
            preObjectBlocks = ret.preObjectBlocks ++ x.preObjectBlocks,
            preMethodBlocks = ret.preMethodBlocks ++ x.preMethodBlocks,
            postMethodBlocks = ret.postMethodBlocks ++ x.postMethodBlocks,
            postObjectBlocks = ret.postObjectBlocks ++ x.postObjectBlocks,
            resources = ret.resources ++ x.resources)
        case _ =>
      }
    }
    return ret
  }
}

object BehaviorEntryPointElementProvider {
  // TODO: could make a plugin out of the following to allow codegen to provide different
  //       component implementations (e.g. singleton vs class)

  @pure def portApiUsage(p: AadlPort): ST = {
    val portType: (AadlType, String) = p match {
      case aep: AadlEventPort => (TypeUtil.EmptyType, "event")
      case aedp: AadlEventDataPort => (aedp.aadlType, "event data")
      case adp: AadlDataPort => (adp.aadlType, "data")
      case _ => halt("Infeasible")
    }
    if (p.direction == Direction.In) {
      val typeName = portType._1.nameProvider.qualifiedReferencedTypeName
      return (
        st"""val apiUsage_${p.identifier}: Option[${typeName}] = api.get_${p.identifier}()
            |api.logInfo(s"Received on ${portType._2} port ${p.identifier}: $${apiUsage_${p.identifier}}")""")
    } else {
      val payload: String =
        if (portType._1.nameProvider.isEmptyType) ""
        else portType._1.nameProvider.example()

      return st"api.put_${p.identifier}($payload)"
    }
  }

  def genComputeMethodBody(optInEventPort: Option[AadlPort],
                           context: AadlThreadOrDevice,
                           includeApiUsage: B,
                           excludeImplementation: B): ST = {

    val inPorts: ISZ[AadlPort] = context.features.filter(f => f.isInstanceOf[AadlPort] && f.asInstanceOf[AadlPort].direction == Direction.In).map(m => m.asInstanceOf[AadlPort])

    val exampleApiGetterUsage: ST =
      st"""// example api usage
          |
          |${(inPorts.map((p: AadlPort) => portApiUsage(p)), "\n")}"""

    if (excludeImplementation) {
      return st""
    }
    else {
      val ret: ST = optInEventPort match {
        case Some(p) =>
          val feedback: ST = p match {
            case a: AadlEventDataPort => st"""api.logInfo(s"  received $$value")"""
            case a: AadlEventPort => st"""api.logInfo("  received event")"""
            case _ => halt("Infeasible as we're generating a handler method for an event port")
          }
          val methodName = genMethodName(EntryPoints.compute, optInEventPort)
          // ideally this would be single ST block, but inserting exampleApiGetUsage as an Option[ST]
          // results in a black line when it's None so using seq expansion instead
          var lines = ISZ[ST](st"""api.logInfo("example $methodName implementation")""", feedback)
          if (includeApiUsage) {
            lines = lines :+ exampleApiGetterUsage
          }
          st"${(lines, "\n")}"
        case _ => // must be time triggered method
          if (includeApiUsage) exampleApiGetterUsage else st""
      }
      return ret
    }
  }

  def genMethodBody(entryPoint: EntryPoints.Type,
                    context: AadlThreadOrDevice,
                    excludeImplementation: B): ST = {

    val outPorts: ISZ[AadlPort] = context.features.filter(f => f.isInstanceOf[AadlPort] && f.asInstanceOf[AadlPort].direction == Direction.Out).map(m => m.asInstanceOf[AadlPort])

    if (excludeImplementation) {
      return st""
    } else {
      val ret: ST = entryPoint match {
        case EntryPoints.compute => halt("Infeasible - call genComputeMethodBody")
        case EntryPoints.initialise =>
          val o: Option[ST] =
            if (outPorts.nonEmpty) Some(
              st"""
                  |${(outPorts.map((p: AadlPort) => portApiUsage(p)), "\n")}""")
            else None()
          st"""// example api usage
              |
              |api.logInfo("Example info logging")
              |api.logDebug("Example debug logging")
              |api.logError("Example error logging")
              |${o}"""
        case _ =>
          st""
      }
      return ret
    }
  }

  def genMethodName(entryPoint: EntryPoints.Type, optInEventPort: Option[AadlPort]): String = {
    entryPoint match {
      case EntryPoints.compute =>
        optInEventPort match {
          case Some(aep: AadlEventPort) => return s"handle_${aep.identifier}"
          case Some(aedp: AadlEventDataPort) => return s"handle_${optInEventPort.get.identifier}"
          case _ => return s"timeTriggered"
        }
      case _ => return entryPoint.name
    }
  }

  def genMethodSignature(entryPoint: EntryPoints.Type, names: NameProvider, optInEventPort: Option[AadlPort]): String = {
    val methodName = genMethodName(entryPoint, optInEventPort)
    val args: ISZ[String] = entryPoint match {
      case EntryPoints.compute =>
        optInEventPort match {
          case Some(aep: AadlEventPort) => ISZ(s"api: ${names.apiOperational}")
          case Some(aedp: AadlEventDataPort) =>
            ISZ(s"api: ${names.apiOperational}", s"value: ${aedp.aadlType.nameProvider.qualifiedReferencedTypeName}")
          case _ => ISZ(s"api: ${names.apiOperational}")
        }
      case EntryPoints.initialise => ISZ(s"api: ${names.apiInitialization}")
      case _ => ISZ(s"api: ${names.apiOperational}")
    }
    return st"def $methodName(${(args, ", ")}): Unit".render
  }

  def genComponentImpl(names: NameProvider, entries: ISZ[BehaviorEntryPointObjectContributions]): ST = {
    @strictpure def wrapST(v: ISZ[ST], sep: String): Option[ST] = if (v.isEmpty) None() else Some(st"${(v, sep)}")

    val methods: ISZ[ST] = for (e <- entries.filter(f => f.isInstanceOf[FullMethodContributions])
      .map((f: BehaviorEntryPointObjectContributions) => f.asInstanceOf[FullMethodContributions])) yield e.method

    val body = wrapST(
      entries.flatMap((f: BehaviorEntryPointObjectContributions) => f.preMethodBlocks) ++
        methods ++
        entries.flatMap((f: BehaviorEntryPointObjectContributions) => f.postMethodBlocks), "\n\n")

    // remove duplicate tags
    val tags: Set[String] = (Set.empty[String] + "#Sireum") ++ entries.flatMap((f: BehaviorEntryPointObjectContributions) => f.tags)

    return (
      st"""// ${(tags.elements, " ")}
          |
          |package ${names.packageName}
          |
          |import org.sireum._
          |import ${names.basePackage}._
          |${StubTemplate.addImports(entries.flatMap((f: BehaviorEntryPointObjectContributions) => f.imports))}
          |
          |${wrapST(entries.flatMap((f: BehaviorEntryPointObjectContributions) => f.preObjectBlocks), "\n\n")}
          |${CommentTemplate.safeToEditComment_scala}
          |object ${names.componentSingletonType} {
          |
          |  $body
          |}
          |${wrapST(entries.flatMap((f: BehaviorEntryPointObjectContributions) => f.postObjectBlocks), "\n\n")}
          |""")
  }
}
