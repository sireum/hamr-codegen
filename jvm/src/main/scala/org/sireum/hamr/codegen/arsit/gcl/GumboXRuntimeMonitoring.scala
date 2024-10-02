// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboXGenUtil.{GGParam, GGPortParam, GGStateVarParam, SymbolKind, inPortsToParams, outPortsToParams}
import org.sireum.hamr.arsit.plugin.{EntryPointProviderPlugin, PlatformProviderPlugin}
import org.sireum.hamr.arsit.templates.{ApiTemplate, EntryPointTemplate}
import org.sireum.hamr.arsit.{EntryPoints, ProjectDirectories}
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.containers.{FileResource, Marker}
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.GclSubclause

object GumboXRuntimeMonitoring {

  // a 'hand-me-down' container to allow the entry point plugin to communicate vision results
  // to the platform provider plugin
  @record class RM_Container(var entrypointKinds: ISZ[ST],
                             var entryPointHandlers: ISZ[ST],
                             var componentModelInfos: ISZ[(String, ST)],
                             var genTestCases: ISZ[ST],
                             var testSuiteCases: ISZ[ST],
                             var testSuiteCaseIds: ISZ[ST],
                             var postInitVisionUpdates: ISZ[ST],
                             var preComputeVisionUpdates: ISZ[ST],
                             var postComputeVisionUpdates: ISZ[ST])

  def handleEntryPointProvider(component: AadlThreadOrDevice,
                               componentNames: NameProvider,

                               entryPointTemplate: EntryPointTemplate,

                               gumboXGen: GumboXGen,
                               containers: GumboXGenUtil.Container,

                               hasStateVariables: B,
                               annexInfo: Option[(GclSubclause, GclSymbolTable)],

                               rmContainer: RM_Container,

                               aadlTypes: AadlTypes,
                               projectDirectories: ProjectDirectories
                              ): EntryPointProviderPlugin.EntryPointContributions = {

    val epCompanionName: String = s"${componentNames.componentSingletonType}_EntryPoint_Companion"
    val bridgeDirectory: String = s"${projectDirectories.utilDir}/${componentNames.packagePath}"

    var resources: ISZ[FileResource] = ISZ()

    val componentMI = GumboXRuntimeMonitoring.getComponentModelInfo(component, componentNames, annexInfo, aadlTypes)
    rmContainer.componentModelInfos = rmContainer.componentModelInfos :+ componentMI

    val preInitMethodName = s"pre_${EntryPoints.initialise.name}"
    val postInitMethodName = s"post_${EntryPoints.initialise.name}"

    val initBody =
      st"""${epCompanionName}.${preInitMethodName}()
          |
          |// implement the following method in 'component':  def ${EntryPoints.initialise.string}(api: ${componentNames.apiInitialization}): Unit = {}
          |component.${EntryPoints.initialise.string}(${ApiTemplate.apiInitializationId})
          |
          |${epCompanionName}.${postInitMethodName}()
          |
          |Art.sendOutput(eventOutPortIds, dataOutPortIds)"""


    val preComputeMethodName = s"pre_${EntryPoints.compute.name}"
    val postComputeMethodName = s"post_${EntryPoints.compute.name}"

    val injectionServiceName: String = s"${componentNames.componentSingletonType}_Injection_Service"
    val injectionProviderName: String = s"${componentNames.componentSingletonType}_Injection_Provider"
    val injectionService: FileResource = {
      val content = st"""// #Sireum
                         |package ${componentNames.packageName}
                         |
                         |import org.sireum._
                         |
                         |${CommentTemplate.doNotEditComment_scala}
                         |
                         |@msig trait ${injectionProviderName} {
                         |  def pre_receiveInput(): Unit
                         |}
                         |
                         |object ${injectionServiceName} {
                         |
                         |  var providers: MSZ[${injectionProviderName}] = MSZ()
                         |
                         |  def register(provider: ${injectionProviderName}): Unit = {
                         |    providers = providers :+ provider
                         |  }
                         |
                         |  def pre_receiveInput(): Unit = {
                         |    for (provider <- providers) {
                         |      provider.pre_receiveInput()
                         |    }
                         |  }
                         |}"""
      ResourceUtil.createResource(s"${bridgeDirectory}/${injectionServiceName}.scala", content, T)
    }
    resources = resources :+ injectionService

    // TODO:
    val computeBody: ST = {
      val s = StringUtil.split_PreserveEmptySegments(entryPointTemplate.defaultComputeBody.render, c => c == '\n')
      var newLines: ISZ[String] = ISZ()
      for (l <- s) {
        val o = ops.StringOps(l)
        if (o.contains("Art.receiveInput")) {
          newLines = newLines :+ s"${injectionServiceName}.pre_receiveInput()\n" :+ l :+ s"\n${epCompanionName}.${preComputeMethodName}()"
        } else if (o.contains("Art.sendOutput")) {
          newLines = newLines :+ s"${epCompanionName}.${postComputeMethodName}()\n\n$l"
        } else {
          newLines = newLines :+ l
        }
      }
      st"${(newLines, "\n")}"
    }

    val gEntryPoint = entryPointTemplate.generateCustomST(
      blocks = ISZ(),
      activateBody = None(),
      initialiseBody = Some(initBody),
      testInitialiseBody = None(),
      computeBody = Some(computeBody),
      testComputeBody = None(),
      deactivateBody = None(),
      finaliseBody = None(),
      recoverBody = None()
    )

    val preContainer: String = "preStateContainer_wL"
    val postContainer: String = "postStateContainer_wL"


    val runtimePackage = s"${componentNames.basePackage}.runtimemonitor"

    val postInitKind = st"${componentNames.instanceName}_postInit"
    val preComputeKind = st"${componentNames.instanceName}_preCompute"
    val postComputeKind = st"${componentNames.instanceName}_postCompute"

    val postInitKindFQ = st"${runtimePackage}.ObservationKind.${postInitKind}"
    val preComputeKindFQ = st"${runtimePackage}.ObservationKind.${preComputeKind}"
    val postComputeKindFQ = st"${runtimePackage}.ObservationKind.${postComputeKind}"

    { // RUNTIME MONITOR -- get updates from post initialization
      var outPorts: ISZ[ST] = ISZ()
      for (o <- containers.outPorts) {
        val p = o.asInstanceOf[GGPortParam]
        if (p.isEvent) {
          outPorts = outPorts :+
            st"""if (postContainer.${p.name}.nonEmpty) {
                |  updates = updates + s"$${bridge_id}_Out_${p.originName}" ~> postContainer.${p.name}.get.string
                |}"""
        } else {
          outPorts = outPorts :+
            st"""updates = updates + s"$${bridge_id}_Out_${p.originName}" ~> postContainer.${p.name}.string"""
        }
      }

      val stateVars: ISZ[ST] = for (sv <- containers.outStateVars) yield st"""updates = updates + s"$${bridge_id}_Out_${sv.originName}" ~> postContainer.${sv.name}.string"""

      rmContainer.postInitVisionUpdates = rmContainer.postInitVisionUpdates :+
        st"""case $postInitKindFQ =>
            |  var updates: Map[String, String] = Map.empty
            |  val postContainer = container.asInstanceOf[${containers.fqPostStateContainerName_PS}]
            |  ${(stateVars ++ outPorts, "\n")}
            |  return updates"""
    }

    val optInit: ST =
      if (gumboXGen.initializeEntryPointHolder.contains(component.path)) {

        val simpleCepPreContainer = GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames)
        st"""val result: B = ${(simpleCepPreContainer, ".")}(postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_PS}])
            |//println(s"${component.identifier}.initialise: Post-condition: $${if (result) "" else "un"}satisfied")
            |return result"""
      } else {
        st"""// checking the post-state values of ${component.identifier}'s initialise entrypoint is not required
            |return T"""
      }

    { // RUNTIME MONITOR -- get update pre compute phase
      var inPorts: ISZ[ST] = ISZ()
      for (o <- containers.inPorts) {
        val p = o.asInstanceOf[GGPortParam]
        if (p.isEvent) {
          inPorts = inPorts :+
            st"""if (preContainer.${p.name}.nonEmpty) {
                |  updates = updates + s"$${bridge_id}_In_${p.originName}" ~> preContainer.${p.name}.get.string
                |}"""
        } else {
          inPorts = inPorts :+
            st"""updates = updates + s"$${bridge_id}_In_${p.originName}" ~> preContainer.${p.name}.string"""
        }
      }

      val stateVars: ISZ[ST] =
        for (sv <- containers.inStateVars) yield st"""updates = updates + s"$${bridge_id}_In_${sv.name}" ~> preContainer.${sv.name}.string"""

      rmContainer.preComputeVisionUpdates = rmContainer.preComputeVisionUpdates :+
        st"""case $preComputeKindFQ =>
            |  var updates: Map[String, String] = Map.empty
            |  val preContainer = container.asInstanceOf[${containers.fqPreStateContainerName_PS}]
            |  ${(stateVars ++ inPorts, "\n")}
            |  return updates"""
    }

    val optPreCompute: ST =
      gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Pre.nonEmpty =>
          val simpleCepPreContainer = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPreContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_PS}])
              |//println(s"${component.identifier}.timeTriggered: Pre-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the pre-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }

    { // RUNTIME MONITOR -- get update post compute phase
      var outPorts: ISZ[ST] = ISZ()
      for (o <- containers.outPorts) {
        val p = o.asInstanceOf[GGPortParam]
        if (p.isEvent) {
          outPorts = outPorts :+
            st"""if (postContainer.${p.name}.nonEmpty) {
                |  updates = updates + s"$${bridge_id}_Out_${p.originName}" ~> postContainer.${p.name}.get.string
                |}"""
        } else {
          outPorts = outPorts :+
            st"""updates = updates + s"$${bridge_id}_Out_${p.originName}" ~> postContainer.${p.name}.string"""
        }
      }

      val stateVars: ISZ[ST] = for (sv <- containers.outStateVars) yield st"""updates = updates + s"$${bridge_id}_Out_${sv.originName}" ~> postContainer.${sv.name}.string"""

      rmContainer.postComputeVisionUpdates = rmContainer.postComputeVisionUpdates :+
        st"""case $postComputeKindFQ =>
            |  var updates: Map[String, String] = Map.empty
            |  val postContainer = container.asInstanceOf[${containers.fqPostStateContainerName_PS}]
            |  ${(stateVars ++ outPorts, "\n")}
            |  return updates"""
    }

    val optPostCompute: ST =
      gumboXGen.computeEntryPointHolder.get(component.path) match {
        case Some(holder) if holder.CEP_Post.nonEmpty =>
          val simpleCepPostContainer = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          st"""val result: B = ${simpleCepPostContainer}(preContainer.get.asInstanceOf[${containers.fqPreStateContainerName_PS}], postContainer.get.asInstanceOf[${containers.fqPostStateContainerName_PS}])
              |//println(s"${component.identifier}.timeTriggered: Post-condition: $${if (result) "" else "un"}satisfied")
              |return result"""
        case _ =>
          st"""// checking the post-state values of ${component.identifier}'s compute entrypoint is not required
              |return T"""
      }

    rmContainer.entrypointKinds = rmContainer.entrypointKinds :+
      st""""${componentNames.instanceName}_postInit"""" :+
      st""""${componentNames.instanceName}_preCompute"""" :+
      st""""${componentNames.instanceName}_postCompute""""

    val entryPointCases =
      st"""case $postInitKindFQ =>
          |  $optInit
          |case $preComputeKindFQ =>
          |  $optPreCompute
          |case $postComputeKindFQ =>
          |  $optPostCompute
          |"""

    rmContainer.entryPointHandlers = rmContainer.entryPointHandlers :+ entryPointCases

    var postInitCases: ISZ[ST] = ISZ()
    var preComputeCases: ISZ[ST] = ISZ()
    var postComputeCases: ISZ[ST] = ISZ()

    gumboXGen.initializeEntryPointHolder.get(component.path) match {
      case Some(holder) =>
        val simple_IEP_Post_container = st"${(GumboXGen.getInitialize_IEP_Post_Container_MethodName(componentNames), ".")}"
        postInitCases = postInitCases :+
          st"""|test(s"${postInitKind}: Check Post-condition$$suffix") {
               ||  val postJson: String = st$${tq}$${postContainer.get}$${tq}.render
               ||  val postContainer = ${componentNames.basePackage}.${containers.postStateContainerJsonTo_PS}(postJson).left
               ||  assert(${simple_IEP_Post_container}(postContainer))
               ||}"""
      case _ =>
    }

    gumboXGen.computeEntryPointHolder.get(component.path) match {
      case Some(holder) =>
        if (holder.CEP_Pre.nonEmpty) {
          val simple_CEP_Pre_container = st"${(GumboXGen.getCompute_CEP_Pre_Container_MethodName(componentNames), ".")}"

          def gen(kind: ST): ST = {
            return (st"""|test(s"$kind: Check Pre-condition$$suffix") {
                         ||  val preJson: String = st$${tq}$${preContainer.get}$${tq}.render
                         ||  val preContainer = ${componentNames.basePackage}.${containers.preStateContainerJsonTo_PS}(preJson).left
                         ||  if (verbose) {
                         ||    println("Pre-State Values:")
                         ||    println(s"  $$$$preContainer")
                         ||  }
                         ||  assert(${simple_CEP_Pre_container}(preContainer))
                         ||}""")
          }

          preComputeCases = preComputeCases :+ gen(preComputeKind)
          postComputeCases = postComputeCases :+ gen(postComputeKind)
        }

        if (holder.CEP_Post.nonEmpty) {
          val simple_CEP_Post_container = st"${(GumboXGen.getCompute_CEP_Post_Container_MethodName(componentNames), ".")}"
          postComputeCases = postComputeCases :+
            st"""|test(s"${postComputeKind}: Check Post-condition$$suffix") {
                 ||  val preJson: String = st$${tq}$${preContainer.get}$${tq}.render
                 ||  val postJson: String = st$${tq}$${postContainer.get}$${tq}.render
                 ||  val preContainer = ${componentNames.basePackage}.${containers.preStateContainerJsonTo_PS}(preJson).left
                 ||  val postContainer = ${componentNames.basePackage}.${containers.postStateContainerJsonTo_PS}(postJson).left
                 ||  if (verbose) {
                 ||    println("Pre-State Values:")
                 ||    println(s"  $$$$preContainer")
                 ||    println("Post-State Values:")
                 ||    println(s"  $$$$postContainer");
                 ||  }
                 ||  assert(${simple_CEP_Post_container}(preContainer, postContainer))
                 ||}"""
        }

        // FIXME: add sporadic cb testing support
        // TODO: disabling this for sept 2023 demo as it's unclear why calling the cb test harness
        //       is useful given that we can't assert anything about what it returns since runtime monitoring
        //       didn't actually call it itself
        if (F && component.isPeriodic()) {
          val methodToCall: String = if (hasStateVariables) "testComputeCBwLV" else "testComputeCBV"

          def gen2(kind: ST): ST = {
            return (st"""|test(s"$kind: Run $methodToCall$$suffix") {
                         ||  val preJson: String = st$${tq}$${preContainer.get}$${tq}.render
                         ||  val preContainer = ${componentNames.basePackage}.${containers.preStateContainerJsonTo_PS}(preJson).left
                         ||  println($methodToCall(preContainer))
                         ||}""")
          }

          preComputeCases = preComputeCases :+ gen2(preComputeKind)
          postComputeCases = postComputeCases :+ gen2(postComputeKind)
        }

      case _ =>
    }

    def trans(kindFQ: ST, kind: ST, cases: ISZ[ST]): Option[ST] = {
      return (
        if (cases.nonEmpty)
          Some(
            st"""case $kindFQ =>
                |  return (st${DSCTemplate.tq}// Begin test cases for ${kind}
                |              |
                |              ${(cases, "\n|\n")}
                |              |// End test cases for ${kind}${DSCTemplate.tq})""")
        else None()
        )
    }

    rmContainer.genTestCases = rmContainer.genTestCases :+
      st"""${trans(postInitKindFQ, postInitKind, postInitCases)}
          |${trans(preComputeKindFQ, preComputeKind, preComputeCases)}
          |${trans(postComputeKindFQ, postComputeKind, postComputeCases)}"""

    // cid will be used in the pattern matching so must be upper-case, otherwise Scala will
    // treat it as a variable/capture
    val cid = ops.StringOps(s"${componentNames.componentSingletonType}_id").firstToUpper
    rmContainer.testSuiteCaseIds = rmContainer.testSuiteCaseIds :+ st"val ${cid} = ${componentNames.archInstanceName}.id"

    val baseName: String =
      if (component.isPeriodic()) ops.ISZOps(GumboXGen.createScalaTestGumboXClassName(componentNames)).last
      else componentNames.testScalaTestName

    val testSuiteCase: ST = {
      st"""case ${cid} =>
          |  val prefix = "${componentNames.componentSingletonType}_RM_TestSuite"
          |  val path = testRoot /+ ISZ(${(for (pn <- componentNames.packageNameI) yield st""""$pn"""", ",")})
          |  val suiteName = genUniqueSuiteName(path, prefix)
          |
          |  val testSuite =
          |    st${DSCTemplate.tq}package ${componentNames.packageName}
          |        |
          |        |import org.sireum._
          |        |import ${componentNames.packageName}._
          |        |
          |        |class $${suiteName} extends ${baseName} {
          |        |  val verbose: B = T
          |        |
          |        |  $${p._2}
          |        |}${DSCTemplate.tq}
          |  val filename = path / s"$${suiteName}.scala"
          |  filename.writeOver(testSuite.render)
          |  println(s"Wrote: $${filename.toUri}")"""
    }

    rmContainer.testSuiteCases = rmContainer.testSuiteCases :+ testSuiteCase

    val epCompanionExt =
      st"""// #Sireum
          |
          |package ${componentNames.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${componentNames.basePackage}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ${epCompanionName} {
          |
          |  ${containers.lastDataPortVars}
          |  var $preContainer: Option[${GumboXGenUtil.genContainerName(componentNames.componentSingletonType, T, T)}] = None()
          |
          |  def ${preInitMethodName}(): Unit = {
          |    // assume/require contracts cannot refer to incoming ports or
          |    // state variables so nothing to do here
          |  }
          |
          |  def ${postInitMethodName}(): Unit = {
          |    // block the component while its post-state values are retrieved
          |    val $postContainer =
          |      ${containers.observePostState_wL()}
          |
          |    // the rest can now be performed via a different thread
          |    ${runtimePackage}.RuntimeMonitor.observeInitialisePostState(${componentNames.archInstanceName}.id, $postInitKindFQ, $postContainer)
          |  }
          |
          |  def ${preComputeMethodName}(): Unit = {
          |    // block the component while its pre-state values are retrieved
          |    $preContainer = Some(
          |      ${containers.observePreState_wL()})
          |
          |    // the rest can now be performed via a different thread
          |    ${runtimePackage}.RuntimeMonitor.observeComputePreState(${componentNames.archInstanceName}.id, $preComputeKindFQ, ${preContainer}.asInstanceOf[Option[art.DataContent]])
          |  }
          |
          |  def ${postComputeMethodName}(): Unit = {
          |    // block the component while its post-state values are retrieved
          |    val $postContainer =
          |      ${containers.observePostState_wL()}
          |
          |    // the rest can now be performed via a different thread
          |    ${runtimePackage}.RuntimeMonitor.observeComputePrePostState(${componentNames.archInstanceName}.id, $postComputeKindFQ, ${preContainer}.asInstanceOf[Option[art.DataContent]], $postContainer)
          |  }
          |}"""

    val path = s"${bridgeDirectory}/${epCompanionName}.scala"
    resources = resources :+ ResourceUtil.createResource(path, epCompanionExt, T)

    val systemTestAPI = genSystemTestApi(componentNames, containers, projectDirectories)

    return EntryPointProviderPlugin.EntryPointContributions(
      imports = ISZ(),
      bridgeCompanionBlocks = ISZ(),
      entryPoint = gEntryPoint,
      resources = resources :+ systemTestAPI
    )
  }

  def genSystemTestSuite(basePackage: String,
                         hasGcl: B,
                         projectDirectories: ProjectDirectories): ISZ[FileResource] = {
    var resources = ISZ[FileResource]()

    val utilpath = s"${projectDirectories.testUtilDir}/${basePackage}"

    val systemTestingInitRuntimeMonitoringPluginContributions: ISZ[ST] = ISZ()
    val systemTestingFinialiseRuntimeMonitoringPluginContributions: ISZ[ST] = ISZ()
    val systemTestingObserveInitialisePostStateRuntimeMonitoringPluginContributions: ISZ[ST] = ISZ(
      st"""assert(GumboXDispatcher.checkContract(observationKind, None(), Some(post)), s"GUMBO post condition for the initialization phase did not hold for $$observationKind")"""
    )
    val systemTestingObserveComputePreStateRuntimeMonitoringPluginContributions: ISZ[ST] = ISZ(
      st"""assert(GumboXDispatcher.checkContract(observationKind, pre, None()), s"GUMBO pre condition for the compute phase did not hold for $$observationKind")"""
    )
    val systemTestingObserveComputePrePostStateRuntimeMonitoringPluginContributions: ISZ[ST] = ISZ(
      st"""assert(GumboXDispatcher.checkContract(observationKind, pre, Some(post)), s"GUMBO post condition for the compute phase did not hold for $$observationKind")"""
    )

    def iszToOpt(sts: ISZ[ST]): Option[ST] = {
      return (if (sts.isEmpty || !hasGcl) None()
      else Some(st"${(sts, "\n")}"))
    }

    val systemTestSuiteSlang =
      st"""// #Sireum
          |
          |package $basePackage
          |
          |import org.sireum._
          |import art.scheduling.Scheduler
          |import art.{Art, DataContent}
          |import $basePackage.SystemTestSuiteSlang._
          |import $basePackage.runtimemonitor.{GumboXDispatcher, ObservationKind, RuntimeMonitor, RuntimeMonitorListener}
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object SystemTestSuiteSlang {
          |  // for now just keep the last post state for a bridge
          |  var runtimeMonitorStream: Map[Art.BridgeId, (ObservationKind.Type, DataContent)] = Map.empty
          |}
          |
          |@msig trait SystemTestSuiteSlang extends RuntimeMonitorListener {
          |
          |  def scheduler: Scheduler
          |
          |  override def init(modelInfo: $basePackage.runtimemonitor.ModelInfo): Unit = {
          |    ${iszToOpt(systemTestingInitRuntimeMonitoringPluginContributions)}
          |  }
          |
          |  override def finalise(): Unit = {
          |    ${iszToOpt(systemTestingFinialiseRuntimeMonitoringPluginContributions)}
          |  }
          |
          |  override def observeInitialisePostState(bridgeId: Art.BridgeId, observationKind: ObservationKind.Type, post: DataContent): Unit = {
          |    runtimeMonitorStream = runtimeMonitorStream + (bridgeId ~> (observationKind, post))
          |
          |    ${iszToOpt(systemTestingObserveInitialisePostStateRuntimeMonitoringPluginContributions)}
          |  }
          |
          |  override def observeComputePreState(bridgeId: art.Art.BridgeId, observationKind: $basePackage.runtimemonitor.ObservationKind.Type, pre: Option[art.DataContent]): Unit = {
          |    ${iszToOpt(systemTestingObserveComputePreStateRuntimeMonitoringPluginContributions)}
          |  }
          |
          |  override def observeComputePrePostState(bridgeId: Art.BridgeId,
          |                                          observationKind: ObservationKind.Type,
          |                                          pre: Option[art.DataContent],
          |                                          post: DataContent): Unit = {
          |    runtimeMonitorStream = runtimeMonitorStream + (bridgeId ~> (observationKind, post))
          |
          |    ${iszToOpt(systemTestingObserveComputePrePostStateRuntimeMonitoringPluginContributions)}
          |  }
          |
          |  def beforeEachSlang(): Unit = {
          |    runtimeMonitorStream = Map.empty
          |
          |    RuntimeMonitor.registerListener(this)
          |
          |    Platform.setup()
          |    Art.initSystemTest(Arch.ad, scheduler)
          |  }
          |
          |  def afterEachSlang(): Unit = {
          |    Art.finalizeSystemTest()
          |    Platform.tearDown()
          |  }
          |
          |  def must_match[A](expected: A, actual: A): Unit = {
          |    assert(expected == actual, s"Expected: $$expected, Actual: $$actual")
          |  }
          |}
          |"""
    val systemTestSuiteSlangPath = s"${utilpath}/SystemTestSuiteSlang.scala"
    resources = resources :+ ResourceUtil.createResource(systemTestSuiteSlangPath, systemTestSuiteSlang, T)

    val systemTestSuite =
      st"""package $basePackage
          |
          |import org.sireum._
          |import org.scalatest.funsuite.AnyFunSuite
          |import org.scalatest.{BeforeAndAfterEach, OneInstancePerTest}
          |import org.sireum.$$internal.MutableMarker
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |abstract class SystemTestSuite extends AnyFunSuite with OneInstancePerTest with BeforeAndAfterEach with SystemTestSuiteSlang {
          |
          |  override protected def beforeEach(): Unit = {
          |    beforeEachSlang()
          |  }
          |
          |  override protected def afterEach(): Unit = {
          |    afterEachSlang()
          |  }
          |
          |  override def string: String = toString()
          |
          |  override def $$clonable: Boolean = false
          |
          |  override def $$clonable_=(b: Boolean): MutableMarker = this
          |
          |  override def $$owned: Boolean = false
          |
          |  override def $$owned_=(b: Boolean): MutableMarker = this
          |
          |  override def $$clone: MutableMarker = this
          |}"""
    val systemTestSuitePath = s"$utilpath/SystemTestSuite.scala"
    resources = resources :+ ResourceUtil.createResource(systemTestSuitePath, systemTestSuite, T)

    return resources
  }

  def genSystemTest(basePackage: String,
                    importRenamings: ISZ[ST],
                    projectDirectories: ProjectDirectories): FileResource = {
    val ret = st"""package $basePackage
                  |
                  |import org.sireum._
                  |import art.Art
                  |import art.scheduling.static._
                  |
                  |${CommentTemplate.safeToEditComment_scala}
                  |
                  |class SystemTests extends SystemTestSuite {
                  |
                  |  // note: this is overriding SystemTestSuite's 'def scheduler: Scheduler'
                  |  //       abstract method
                  |  var scheduler: StaticScheduler = Schedulers.getStaticSchedulerH(MNone())
                  |
                  |  def compute(isz: ISZ[Command]): Unit = {
                  |    scheduler = scheduler(commandProvider = ISZCommandProvider(isz :+ Stop()))
                  |
                  |    Art.computePhase(scheduler)
                  |  }
                  |
                  |  override def beforeEach(): Unit = {
                  |    // uncomment the following to disable the various guis
                  |    //System.setProperty("java.awt.headless", "true")
                  |
                  |    // uncomment the following to suppress (or potentially redirect) ART's log stream
                  |    //art.ArtNative_Ext.logStream = new java.io.PrintStream(new java.io.OutputStream {
                  |    //  override def write(b: Int): Unit = {}
                  |    //})
                  |
                  |    // uncomment the following to suppress (or potentially redirect) the static scheduler's log stream
                  |    //art.scheduling.static.StaticSchedulerIO_Ext.logStream = new java.io.PrintStream(new java.io.OutputStream {
                  |    //  override def write(b: Int): Unit = {}
                  |    //})
                  |
                  |    super.beforeEach()
                  |  }
                  |
                  |  // Suggestion: add the following import renamings of the components' SystemTestAPIs,
                  |  //             replacing nickname with shortened versions that are easier to reference
                  |  ${(importRenamings, "\n")}
                  |
                  |  test("Example system test") {
                  |    // run the initialization phase
                  |    Art.initializePhase(scheduler)
                  |
                  |    // run components' compute entrypoints through one hyper-period
                  |    compute(ISZ(Hstep(1)))
                  |
                  |    // use the component SystemTestAPIs' put methods to change the prestate values for components
                  |    // TODO
                  |
                  |    // run another hyper-period
                  |    compute(ISZ(Hstep(1)))
                  |
                  |    // use the component SystemTestAPIs' check or get methods to check the poststate values for components
                  |    // TODO
                  |  }
                  |}
                  |"""
    val stsuitePath = s"${projectDirectories.testSystemDir}/${basePackage}/SystemTests.scala"
    return ResourceUtil.createResource(stsuitePath, ret, F)
  }

  def genSystemTestApi(componentNames: NameProvider,
                       containers: GumboXGenUtil.Container,
                       projectDirectories: ProjectDirectories): FileResource = {

    val inParams = GumboXGenUtil.sortParam(containers.inPorts ++ containers.inStateVars.asInstanceOf[ISZ[GGParam]])
    var putMethodParams: ISZ[ST] = ISZ()
    var putMethodBlocks: ISZ[ST] = ISZ()
    var putMethods: ISZ[ST] = ISZ()
    var hasStateVars: B = F
    for(p <- inParams) {
      hasStateVars = hasStateVars || p.isInstanceOf[GGStateVarParam]
      putMethods = putMethods :+ p.setter
      putMethodParams = putMethodParams :+ p.getParamDef
      putMethodBlocks = putMethodBlocks :+
        st"put_${if(p.isInstanceOf[GGStateVarParam]) p.name else p.originName}(${p.name})"
    }

    val concretePut =
      st"""/** helper method to set the values of all incoming ports${if (hasStateVars) " and state variables" else ""}
          |  ${(GumboXGenUtil.paramsToComment(inParams), "\n")}
          |  */
          |def put_concrete_inputs(${(putMethodParams, ",\n")}): Unit = {
          |  ${(putMethodBlocks, "\n")}
          |}
          |
          |${(putMethods, "\n\n")}"""

    var check_parameters: ISZ[ST] = ISZ()
    var checks: ISZ[ST] = ISZ()
    var getters: ISZ[ST] = ISZ()
    for(p <- GumboXGenUtil.sortParam(containers.outPorts ++ containers.outStateVars.asInstanceOf[ISZ[GGParam]])) {
      p match {
        case port: GGPortParam =>
          getters = getters :+
            st"""def get_${p.name}(): ${p.slangType} = {
                |  return fetchContainer().${p.name}
                |}"""
          check_parameters = check_parameters :+ p.getParamDef
          checks = checks :+
            st"""val actual_${p.originName} = get_${p.name}()
                |if (${p.name} != actual_${p.originName}) {
                |  failureReasons = failureReasons :+ st"'${p.originName}' did not match expected.  Expected: $$${p.name}, Actual: $$actual_${p.originName}"
                |}"""
        case statevar: GGStateVarParam =>
          // TODO: could get these from the container as well
          getters = getters :+ statevar.getter
          check_parameters = check_parameters :+ p.getParamDef
          checks = checks :+
            st"""val actual_${statevar.originName} = get_${p.name}()
                |if (${p.name} != actual_${statevar.originName}) {
                |  failureReasons = failureReasons :+ st"'${p.originName}' did not match expected.  Expected: $$${p.name}, Actual: $$actual_${p.originName}"
                |}"""
      }
    }

    val checkConcreteOutputs =
      st"""def fetchContainer(): ${containers.fqPostStateContainerName_PS} = {
          |  if (runtimeMonitorStream.contains(${componentNames.archInstanceName}.id)) {
          |    val (_, postContainer_) = runtimeMonitorStream.get(${componentNames.archInstanceName}.id).get
          |    return postContainer_.asInstanceOf[${containers.fqPostStateContainerName_PS}]
          |  }
          |  else {
          |    assert(F, s"No post state recorded for $${${componentNames.archInstanceName}.name}")
          |    halt(s"No post state recorded for $${${componentNames.archInstanceName}.name}")
          |  }
          |}
          |
          |def check_concrete_outputs(${(check_parameters, ",\n")}): Unit = {
          |  var failureReasons: ISZ[ST] = ISZ()
          |
          |  ${(checks, "\n")}
          |
          |  assert(failureReasons.isEmpty, st"$${(failureReasons, "\n")}".render)
          |}
          |
          |${(getters, "\n\n")}"""

    val objectName = s"${componentNames.componentSingletonType}_SystemTestAPI"
    val testApi =
      st"""// #Sireum
          |
          |package ${componentNames.packageName}
          |
          |import org.sireum._
          |import art._
          |import ${componentNames.basePackage}.SystemTestSuiteSlang.runtimeMonitorStream
          |import ${componentNames.basePackage}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object $objectName {
          |  ${concretePut}
          |
          |  ${checkConcreteOutputs}
          |}"""
    val objectPath = s"${projectDirectories.testUtilDir}/${componentNames.packagePath}/${objectName}.scala"
    return ResourceUtil.createResource(objectPath, testApi, T)
  }

  def handlePlatformProviderPlugin(rmContainer: RM_Container,
                                   hasGcl: B,
                                   basePackageName: String,
                                   projectDirectories: ProjectDirectories): ISZ[PlatformProviderPlugin.PlatformContributions] = {
    val runtimePath = s"${projectDirectories.utilDir}/${basePackageName}/runtimemonitor"

    var resources: ISZ[FileResource] = ISZ()


    resources = resources ++ GumboXRuntimeMonitoring.genSystemTestSuite(basePackageName, hasGcl, projectDirectories)

    val runtimePackage = s"${basePackageName}.runtimemonitor"

    val observationKind =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import ${basePackageName}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@enum object ObservationKind {
          |  ${(rmContainer.entrypointKinds, "\n")}
          |}
          |"""
    resources = resources :+ ResourceUtil.createResourceH(s"${runtimePath}/ObservationKind.scala", observationKind, T, T)

    val process =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import ${basePackageName}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object GumboXDispatcher {
          |  def checkContract(observationKind: ObservationKind.Type, preContainer: Option[art.DataContent], postContainer: Option[art.DataContent]): B = {
          |    observationKind match {
          |      ${(rmContainer.entryPointHandlers, "\n")}
          |      case _ => halt("Infeasible")
          |    }
          |  }
          |
          |  def genTestSuite(testCases: ISZ[(Z, ISZ[ST])]): Unit = {
          |    val tq = ${DSCTemplate.tqq}
          |
          |    val testRoot = Os.path(".") / "src" / "test" / "bridge"
          |
          |    ${(rmContainer.testSuiteCaseIds, "\n")}
          |
          |    def genUniqueSuiteName(path: Os.Path, prefix: String): String = {
          |      var i = 0
          |      while(true) {
          |        val cand = path / s"$${prefix}_$${i}.scala"
          |        if (!cand.exists) {
          |          return s"$${prefix}_$${i}"
          |        }
          |        i = i + 1
          |      }
          |      halt("Infeasible")
          |    }
          |
          |    for (p <- testCases) {
          |      art.Art.BridgeId.fromZ(p._1) match {
          |        ${(rmContainer.testSuiteCases, "\n")}
          |        case x => halt(s"Infeasible bridge id: $$x")
          |      }
          |    }
          |  }
          |
          |  def genTestCase(observationKind: ObservationKind.Type, preContainer: Option[String], postContainer: Option[String], testNameSuffix: Option[String]): ST = {
          |    val tq = ${DSCTemplate.tqq}
          |    val suffix: String =
          |      if (testNameSuffix.nonEmpty) testNameSuffix.get
          |      else ""
          |
          |    observationKind match {
          |      ${(rmContainer.genTestCases, "\n")}
          |      case _ => return st"// TODO $${observationKind}"
          |    }
          |  }
          |
          |  def getUpdates(bridge_id: art.Art.BridgeId, observationKind: ObservationKind.Type, container: art.DataContent): Map[String, String] = {
          |    observationKind match {
          |      ${(rmContainer.postInitVisionUpdates, "\n")}
          |      ${(rmContainer.preComputeVisionUpdates, "\n")}
          |      ${(rmContainer.postComputeVisionUpdates, "\n")}
          |      case _ => return Map.empty
          |    }
          |  }
          |}"""

    resources = resources :+ ResourceUtil.createResource(s"${runtimePath}/GumboXDispatcher.scala", process, T)

    val modelInfo = GumboXRuntimeMonitoring.genModelInfo(rmContainer.componentModelInfos, runtimePackage, basePackageName)
    resources = resources :+ ResourceUtil.createResource(s"${runtimePath}/ModelInfo.scala", modelInfo, T)

    val platformSetupBlocks = ISZ(st"${runtimePackage}.RuntimeMonitor.init(${runtimePackage}.ModelInfo.modelInfo)")
    val platformTeardownBlocks = ISZ(st"${runtimePackage}.RuntimeMonitor.finalise()")

    val runtimeMonitor: ST =
      st"""// #Sireum
          |
          |package $runtimePackage
          |
          |import org.sireum._
          |import art.Art.BridgeId
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@msig trait RuntimeMonitorListener {
          |  def init(modelInfo: ModelInfo): Unit
          |
          |  def finalise(): Unit
          |
          |  /**
          |    * Called before the initialise entrypoint calls sendOutput
          |    */
          |  def observeInitialisePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit
          |
          |  /**
          |    * Called after the compute entrypoint calls receiveInput and before it
          |    * invokes the behavior code
          |    */
          |  def observeComputePreState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit
          |
          |  /**
          |    * Called after the compute entrypoint calls receiveInput and before it
          |    * invokes the behavior code
          |    */
          |  def observeComputePrePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit
          |}
          |
          |@ext object RuntimeMonitor {
          |
          |  def registerListener(listener: RuntimeMonitorListener): Unit = $$
          |
          |  def init(modelInfo: ModelInfo): Unit = $$
          |
          |  def finalise(): Unit = $$
          |
          |  def observeInitialisePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = $$
          |
          |  def observeComputePreState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit = $$
          |
          |  def observeComputePrePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit = $$
          |}
          |"""
    val rmpath = s"${runtimePath}/RuntimeMonitor.scala"
    resources = resources :+ ResourceUtil.createResource(rmpath, runtimeMonitor, T)

    val drmMarker = Marker.createMarker("RUNTIME MONITORING")

    val runtimeMonitorExt: ST =
      st"""package $runtimePackage
          |
          |import org.sireum._
          |import art.Art._
          |
          |import java.awt.GraphicsEnvironment
          |
          |${CommentTemplate.safeToEditComment_scala}
          |
          |object RuntimeMonitor_Ext {
          |
          |  /** you use the java.awt.headless system property to enable/disable guis.
          |    * e.g. in scala test context
          |    *
          |    *   override def beforeEach(): Unit = {
          |    *     System.setProperty("java.awt.headless", "true")
          |    *     super.beforeEach()
          |    *   }
          |    *
          |    */
          |
          |  val baseListeners: ISZ[RuntimeMonitorListener] =
          |    if (GraphicsEnvironment.isHeadless) ISZ()
          |    else {
          |      ISZ(
          |        // add/remove listeners here
          |
          |
          |        ${drmMarker.beginMarker}
          |
          |        // if you don't want to use the following runtime monitors then surround this marker block
          |        // with a block comment /** .. **/ to prevent codegen from emitting an error if it's rerun
          |
          |        ${if (!hasGcl) "// " else ""}new GumboXRuntimeMonitor_Ext(),
          |
          |        new HamrVisionRuntimeMonitor(HamrVision.cp)
          |
          |        ${drmMarker.endMarker}
          |      )
          |    }
          |
          |  var externalListeners: ISZ[RuntimeMonitorListener] = ISZ()
          |
          |  def registerListener(listener: RuntimeMonitorListener): Unit = {
          |    externalListeners = externalListeners :+ listener
          |  }
          |
          |  def init(modelInfo: ModelInfo): Unit = {
          |    for (l <- baseListeners) {
          |      l.init(modelInfo)
          |    }
          |    for (l <- externalListeners) {
          |      l.init(modelInfo)
          |    }
          |  }
          |
          |  def finalise(): Unit = {
          |    for (l <- baseListeners) {
          |      l.finalise()
          |    }
          |
          |    for (l <- externalListeners) {
          |      l.finalise()
          |    }
          |    // deregister external listeners
          |    externalListeners = ISZ()
          |  }
          |
          |  def observeInitialisePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = {
          |    for (l <- baseListeners) {
          |      l.observeInitialisePostState(bridgeId, observationKind, post)
          |    }
          |
          |    for (l <- externalListeners) {
          |      l.observeInitialisePostState(bridgeId, observationKind, post)
          |    }
          |  }
          |
          |  def observeComputePreState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit = {
          |    for (l <- baseListeners) {
          |      l.observeComputePreState(bridgeId, observationKind, pre)
          |    }
          |
          |    for (l <- externalListeners) {
          |      l.observeComputePreState(bridgeId, observationKind, pre)
          |    }
          |  }
          |
          |  def observeComputePrePostState(bridgeId: BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit = {
          |    for (l <- baseListeners) {
          |      l.observeComputePrePostState(bridgeId, observationKind, pre, post)
          |    }
          |
          |    for (l <- externalListeners) {
          |      l.observeComputePrePostState(bridgeId, observationKind, pre, post)
          |    }
          |  }
          |}"""

    val rmpathExt = s"${runtimePath}/RuntimeMonitor_Ext.scala"
    resources = resources :+ ResourceUtil.createResourceWithMarkers(rmpathExt, runtimeMonitorExt, ISZ(drmMarker), F)

    val gui: ST =
      st"""// #Sireum
          |
          |package ${runtimePackage}
          |
          |import org.sireum._
          |import ${basePackageName}.Schedulers
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object GumboXRuntimeMonitor {
          |  // assign a new map to threadNickNames to provide custom nicknames (e.g. shorter)
          |  // for 1 or more of the threads in the system
          |  var threadNickNames: Map[art.Art.BridgeId, String] = Schedulers.revThreadNickNames
          |}
          |"""
    val guipath = s"${runtimePath}/GumboXRuntimeMonitor.scala"
    resources = resources :+ ResourceUtil.createResource(guipath, gui, T)

    val gxrm: ST = st"""package ${basePackageName}.runtimemonitor
                       |
                       |import art.Art.BridgeId
                       |import ${basePackageName}.JSON
                       |import org.sireum.$$internal.MutableMarker
                       |import org.sireum._
                       |
                       |import java.awt.event.WindowEvent
                       |import java.awt.{BorderLayout, Dimension}
                       |import javax.swing._
                       |import javax.swing.table.AbstractTableModel
                       |
                       |// Do not edit this file as it will be overwritten if HAMR codegen is rerun
                       |
                       |class GumboXRuntimeMonitor_Ext extends JFrame with RuntimeMonitorListener {
                       |
                       |  val testDir = Os.path(".") / "src" / "test" / "bridge" / "${basePackageName}"
                       |
                       |  var jtable: JTable = _
                       |  var model: TableModel = _
                       |  var scrollToBottom: B = T
                       |  val threadNickNames = GumboXRuntimeMonitor.threadNickNames
                       |
                       |  def init(modelInfo: ModelInfo): Unit = {
                       |    this.setTitle("GumboX Runtime Monitor")
                       |
                       |    model = new TableModel()
                       |    jtable = new JTable()
                       |    jtable.setModel(model)
                       |
                       |    val js = new JScrollPane(jtable)
                       |    js.setVisible(true)
                       |    add(js, BorderLayout.CENTER)
                       |
                       |    val btnGenTestSuite = new JButton("Generate TestSuite")
                       |    btnGenTestSuite.addActionListener(e => {
                       |      if (jtable.getSelectedRows.nonEmpty) {
                       |        var testCases: Map[Z, ISZ[ST]] = Map.empty
                       |
                       |        for (row <- jtable.getSelectedRows) {
                       |          val data = model.getRow(row)
                       |          val id = data.bridgeId.toZ
                       |          testCases = testCases + id ~>
                       |            (testCases.getOrElse(id, ISZ[ST]()) :+
                       |              GumboXDispatcher.genTestCase(data.observationKind, data.pre, data.post, Some(s": $$row")))
                       |        }
                       |        GumboXDispatcher.genTestSuite(testCases.entries)
                       |      }
                       |    })
                       |    /*
                       |    val btnGenTestCase = new JButton("Generate Test Case")
                       |
                       |    btnGenTestCase.addActionListener(e => {
                       |      if (jtable.getSelectedRow >= 0) {
                       |        val data = model.getRow(jtable.getSelectedRow)
                       |
                       |        if (data.observationKind.string.native.contains("post")) {
                       |          val testCase = GumboXDispatcher.genTestCase(data.observationKind, data.pre, data.post, None())
                       |
                       |          val clip = Toolkit.getDefaultToolkit.getSystemClipboard
                       |          val strse1 = new StringSelection(testCase.render.native)
                       |          clip.setContents(strse1, strse1)
                       |
                       |          val txt = st${DSCTemplate.tq}<html><pre>$${testCase.render}</pre></html>${DSCTemplate.tq}
                       |          val lbl = new JLabel(txt.render.native)
                       |
                       |          val viz = new JFrame()
                       |          viz.add(lbl)
                       |
                       |          viz.pack()
                       |          viz.setVisible(true)
                       |        }
                       |      }
                       |    })
                       |
                       |    val btnVisualize = new JButton("Visualize")
                       |
                       |    btnVisualize.addActionListener(e => {
                       |      if (jtable.getSelectedRow >= 0) {
                       |        val data = model.getRow(jtable.getSelectedRow)
                       |
                       |        val preOpt: Option[ST] = if (data.pre.nonEmpty) Some(st"Pre: $${JSON.to_artDataContent(data.pre.get).left}") else None()
                       |        val postOpt: Option[ST] = if (data.post.nonEmpty) Some(st"Post: $${JSON.to_artDataContent(data.post.get).left}") else None()
                       |
                       |        val txt =
                       |          st${DSCTemplate.tq}<html>
                       |              |  <pre>
                       |              |    Component: $${data.bridgeId}
                       |              |    Observation Kind: $${data.observationKind}
                       |              |    <hr>
                       |              |    $${preOpt}
                       |              |    $${postOpt}
                       |              |  </pre>
                       |              |</html>${DSCTemplate.tq}
                       |
                       |        val lbl = new JLabel(txt.render.native)
                       |
                       |        val viz = new JFrame()
                       |        viz.add(lbl)
                       |
                       |        viz.pack()
                       |        viz.setVisible(true)
                       |      }
                       |    })
                       |    */
                       |    val jpbutton = new JPanel()
                       |
                       |    val chkScroll = new JCheckBox("Scroll to bottom")
                       |    chkScroll.setSelected(scrollToBottom)
                       |    chkScroll.addActionListener(e =>
                       |      scrollToBottom = chkScroll.isSelected
                       |    )
                       |    jpbutton.add(chkScroll)
                       |
                       |    //jpbutton.add(Box.createRigidArea(new Dimension(10, 0)))
                       |    jpbutton.setLayout(new BoxLayout(jpbutton, BoxLayout.LINE_AXIS))
                       |    jpbutton.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10))
                       |    jpbutton.add(Box.createHorizontalGlue())
                       |
                       |    jpbutton.add(btnGenTestSuite)
                       |    jpbutton.add(Box.createRigidArea(new Dimension(10, 0)))
                       |
                       |    //jpbutton.add(btnGenTestCase)
                       |    //jpbutton.add(Box.createRigidArea(new Dimension(10, 0)))
                       |
                       |    //jpbutton.add(btnVisualize)
                       |
                       |
                       |    add(jpbutton, BorderLayout.PAGE_END)
                       |
                       |    pack()
                       |    setResizable(true)
                       |    setLocation(800, 0)
                       |    setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
                       |    setVisible(true)
                       |  }
                       |
                       |  def finalise(): Unit = {
                       |    this.dispatchEvent(new WindowEvent(this, WindowEvent.WINDOW_CLOSING))
                       |  }
                       |
                       |  def observeInitialisePostState(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = {
                       |    SwingUtilities.invokeLater(() => dispatch(bridge, observationKind, None(), Some(post)))
                       |  }
                       |
                       |  def observeComputePreState(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent]): Unit = {
                       |    SwingUtilities.invokeLater(() => dispatch(bridge, observationKind, pre, None()))
                       |  }
                       |
                       |  def observeComputePrePostState(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: art.DataContent): Unit = {
                       |    SwingUtilities.invokeLater(() => dispatch(bridge, observationKind, pre, Some(post)))
                       |  }
                       |
                       |  def dispatch(bridgeId: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[art.DataContent], post: Option[art.DataContent]): Unit = {
                       |    val componentName = threadNickNames.get(bridgeId) match {
                       |      case Some(nickName) => nickName.native
                       |      case _ => bridgeId.string.native
                       |    }
                       |    val s = observationKind.string.native
                       |    val simpleKind = s.substring(s.lastIndexOf("_") + 1, s.length)
                       |    model.addRow(Row(bridgeId, observationKind,
                       |      componentName, simpleKind,
                       |      GumboXDispatcher.checkContract(observationKind, pre, post),
                       |      if (pre.nonEmpty) Some(JSON.from_artDataContent(pre.get, T)) else None(),
                       |      if (post.nonEmpty) Some(JSON.from_artDataContent(post.get, T)) else None()))
                       |    if (scrollToBottom) {
                       |      jtable.scrollRectToVisible(jtable.getCellRect(jtable.getRowCount() - 1, 0, true));
                       |    }
                       |  }
                       |
                       |  override def string: String = toString()
                       |
                       |  override def $$clonable: Boolean = false
                       |
                       |  override def $$clonable_=(b: Boolean): MutableMarker = this
                       |
                       |  override def $$owned: Boolean = false
                       |
                       |  override def $$owned_=(b: Boolean): MutableMarker = this
                       |
                       |  override def $$clone: MutableMarker = this
                       |}
                       |
                       |case class Row(bridgeId: BridgeId, observationKind: ObservationKind.Type,
                       |               componentName: String, simpleKind: String, result: Boolean, pre: Option[String], post: Option[String])
                       |
                       |class TableModel() extends AbstractTableModel {
                       |  val columnNames = Array("Component", "Kind", "Satisified")
                       |
                       |  var data: ISZ[Row] = ISZ()
                       |
                       |  def addRow(row: Row): Unit = {
                       |    data = data :+ row
                       |    fireTableRowsInserted(data.size.toInt - 1, data.size.toInt - 1)
                       |  }
                       |
                       |  def getRow(row: Int): Row = {
                       |    return data(row)
                       |  }
                       |
                       |  override def getRowCount: Int = {
                       |    return data.size.toInt
                       |  }
                       |
                       |  override def getColumnCount: Int = {
                       |    return columnNames.length
                       |  }
                       |
                       |  override def getColumnName(column: Int): java.lang.String = {
                       |    return columnNames(column)
                       |  }
                       |
                       |  override def getValueAt(rowIndex: Int, columnIndex: Int): Object = {
                       |    return columnIndex match {
                       |      case 0 => data(rowIndex).componentName.native
                       |      case 1 => data(rowIndex).simpleKind.native
                       |      case 2 => data(rowIndex).result.string.native
                       |      case _ => halt("Infeasible")
                       |    }
                       |  }
                       |}
                       |"""
    val gxrmpath = s"${runtimePath}/GumboXRuntimeMonitor_Ext.scala"
    resources = resources :+ ResourceUtil.createResource(gxrmpath, gxrm, T)

    val hamrVision = getHamrVision(basePackageName)
    val hamrVision_Path = s"${runtimePath}/HamrVision.scala"
    resources = resources :+ ResourceUtil.createResource(hamrVision_Path, hamrVision, T)

    val hamrVision_Ext = getHamrVision_Ext(basePackageName)
    val hamrVision_Ext_Path = s"${runtimePath}/HamrVisionRuntimeMonitorI_Ext.scala"
    resources = resources :+ ResourceUtil.createResource(hamrVision_Ext_Path, hamrVision_Ext, T)

    return ISZ(
      PlatformProviderPlugin.PlatformSetupContributions(imports = ISZ(), blocks = platformSetupBlocks, resources = resources),
      PlatformProviderPlugin.PlatformTearDownContributions(imports =ISZ(), blocks = platformTeardownBlocks, resources = ISZ()))
  }

  def genModelInfo(componentInfos: ISZ[(String, ST)], runtimePackage: String, basePackage: String): ST = {
    val ret =
      st"""// #Sireum
          |package ${runtimePackage}
          |
          |import org.sireum._
          |import ${basePackage}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ModelInfo {
          |  ${(for (i <- componentInfos) yield i._2, "\n\n")}
          |
          |  val modelInfo: ModelInfo =
          |    ModelInfo(ISZ(
          |     ${(for (i <- componentInfos) yield i._1, ",\n")}))
          |}
          |
          |@datatype class ModelInfo(val components: ISZ[Component])
          |
          |@datatype class Component(val name: String,
          |                          val id: Z,
          |                          val dispatchProtocol: iDispatchProtocol.Type,
          |                          val state: ISZ[StateElement])
          |
          |@enum object iDispatchProtocol {
          |  "Sporadic"
          |  "Periodic"
          |}
          |
          |@enum object StateDirection {
          |  "In"
          |  "Out"
          |}
          |
          |@sig trait StateElement {
          |  def name: String
          |
          |  def id: Z
          |
          |  def slangType: String
          |
          |  def direction: StateDirection.Type
          |}
          |
          |@enum object PortKind {
          |  "Data"
          |  "Event"
          |  "EventData"
          |}
          |
          |@datatype class Port(val name: String,
          |                     val id: Z,
          |                     val kind: PortKind.Type,
          |                     val direction: StateDirection.Type,
          |                     val slangType: String) extends StateElement
          |
          |@datatype class StateVariable(val name: String,
          |                              val id: Z,
          |                              val direction: StateDirection.Type,
          |                              val slangType: String) extends StateElement
          |"""
    return ret
  }


  @strictpure def getComponentModelInfoName(componentNames: NameProvider): String =
    s"${componentNames.componentSingletonType}_MI"

  def getComponentModelInfo(component: AadlThreadOrDevice, componentNames: NameProvider, annexInfo: Option[(GclSubclause, GclSymbolTable)], aadlTypes: AadlTypes): (String, ST) = {

    val inStateVars = GumboXGenUtil.stateVarsToParams(componentNames, annexInfo, T, aadlTypes).asInstanceOf[ISZ[GGStateVarParam]]
    val outStateVars = GumboXGenUtil.stateVarsToParams(componentNames, annexInfo, F, aadlTypes).asInstanceOf[ISZ[GGStateVarParam]]
    val inPorts = GumboXGenUtil.inPortsToParams(component, componentNames)
    val outPorts = GumboXGenUtil.outPortsToParams(component, componentNames)

    var stateElements = ISZ[ST]()
    for (sv <- inStateVars ++ outStateVars) {
      stateElements = stateElements :+
        st"""StateVariable(
            |  name = "${sv.name}",
            |  id = ${sv.id},
            |  direction = StateDirection.${if (sv.kind == SymbolKind.StateVarPre) "In" else "Out"},
            |  slangType = "${sv.aadlType.nameProvider.qualifiedReferencedTypeName}")"""
    }
    for (f <- component.features if f.isInstanceOf[AadlPort]) {
      val (kind, direction, slangType): (String, String, String) = f match {
        case i: AadlEventPort => ("PortKind.Event", s"StateDirection.${i.direction.name}", "Option[Empty]")
        case i: AadlEventDataPort => ("PortKind.EventData", s"StateDirection.${i.direction.name}", i.aadlType.nameProvider.qualifiedReferencedTypeName)
        case i: AadlDataPort => ("PortKind.Data", s"StateDirection.${i.direction.name}", i.aadlType.nameProvider.qualifiedReferencedTypeName)
      }
      stateElements = stateElements :+
        st"""Port(
            |  name = "${f.identifier}",
            |  id = ${componentNames.archInstanceName}.${f.identifier}.id.toZ,
            |  kind = $kind,
            |  direction = ${direction},
            |  slangType = "")"""
    }

    val cmiName = getComponentModelInfoName(componentNames)
    val ret =
      st"""val $cmiName : Component =
          |  Component(
          |    name = "${componentNames.componentSingletonType}",
          |    id = ${componentNames.archInstanceName}.id.toZ,
          |    dispatchProtocol = iDispatchProtocol.${if (component.isPeriodic()) "Periodic" else "Sporadic"},
          |    state = ISZ(
          |      ${(stateElements, ",\n")}))"""

    return (cmiName, ret)
  }

  def getHamrVision(basePackage: String): ST = {
    val ret = st"""// #Sireum
                  |
                  |package ${basePackage}.runtimemonitor
                  |
                  |import org.sireum._
                  |import art.scheduling.static.CommandProvider
                  |
                  |${CommentTemplate.doNotEditComment_scala}
                  |
                  |object HamrVision {
                  |
                  |  var cp: MOption[CommandProvider] = MNone()
                  |
                  |  def getCommandProvider(): CommandProvider = {
                  |    cp = MSome(HamrVisionRuntimeMonitorI.getCommandProvider())
                  |    return cp.get
                  |  }
                  |}
                  |
                  |@ext object HamrVisionRuntimeMonitorI {
                  |  def getCommandProvider(): CommandProvider = $$
                  |}"""
    return ret
  }

  def getHamrVision_Ext(basePackage: String): ST = {
    val ret =
      st"""package ${basePackage}.runtimemonitor
          |
          |import art.scheduling.static._
          |import art.{Art, DataContent}
          |import org.sireum.$$internal.MutableMarker
          |import org.sireum._
          |import org.sireum.hamr.vision.treetable.{Category, Entry, JTreeTable, Row => hvRow}
          |import org.sireum.hamr.vision.value.{StringValue, Value}
          |
          |import java.awt.{BorderLayout, Dimension, GridBagConstraints, GridBagLayout}
          |import java.util.concurrent.{Executors, LinkedBlockingDeque, TimeUnit}
          |import javax.swing._
          |import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
          |import scala.jdk.CollectionConverters._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object HamrVisionRuntimeMonitorI_Ext {
          |  def getCommandProvider(): CommandProvider = {
          |    return new HamrVisionCommandProvider()
          |  }
          |}
          |
          |class HamrVisionCommandProvider extends JPanel with InfoCommandProvider {
          |
          |  var threadNickNames: Map[String, art.Art.BridgeId] = Map.empty
          |
          |  var numSlots: Z = 0
          |
          |  var displayOrder: ISZ[Art.BridgeId] = IS()
          |
          |  var clearColorsCallback: () => Unit = _
          |
          |  val blockingQueue: LinkedBlockingDeque[Command] = new LinkedBlockingDeque[Command](1)
          |
          |  val jlNextThread = new JLabel()
          |  val jlSlot = new JLabel()
          |  val jlDomain = new JLabel()
          |  val jlHyperPeriod = new JLabel()
          |
          |  var lastHP = z"0"
          |  var cleared: B = F
          |
          |  def init(threadNickNames: Map[String, Art.BridgeId],
          |           numSlots: Z,
          |           displayOrder: ISZ[Art.BridgeId]): CommandProvider = {
          |    this.threadNickNames = threadNickNames
          |    this.numSlots = numSlots
          |    this.displayOrder = displayOrder
          |
          |    var y = 0
          |    var x = 0
          |    val gbc = new GridBagConstraints()
          |
          |    def gadd(panel: JComponent, jcomponent: JComponent): Unit = {
          |      gbc.gridy = y
          |      gbc.gridx = x
          |      gbc.anchor = if (x == 0) GridBagConstraints.EAST else GridBagConstraints.WEST
          |      panel.add(jcomponent, gbc)
          |      y = y + x
          |      x = (x + 1) % 2
          |    }
          |
          |    val leftPanel = new JPanel()
          |
          |    {
          |      x = 0
          |      y = 0
          |
          |      leftPanel.setLayout(new GridBagLayout())
          |
          |      val spnHStep = new JSpinner(new SpinnerNumberModel(1, 1, 100, 1))
          |
          |      val btnHStep: JButton = new JButton("Hyper period Step")
          |      btnHStep.addActionListener(e => {
          |        val numSteps = Z(spnHStep.getValue.string).get
          |        blockingQueue.offer(Hstep(numSteps), 100, TimeUnit.MILLISECONDS)
          |      })
          |      gadd(leftPanel, btnHStep)
          |      gadd(leftPanel, spnHStep)
          |
          |      val spnSstep = new JSpinner(new SpinnerNumberModel(1, 1, 100, 1))
          |
          |      val btnSstep: JButton = new JButton("Slot Step")
          |      btnSstep.addActionListener(e => {
          |        val numSteps = Z(spnSstep.getValue.string).get
          |        blockingQueue.offer(Sstep(numSteps), 100, TimeUnit.MILLISECONDS)
          |      })
          |      gadd(leftPanel, btnSstep)
          |      gadd(leftPanel, spnSstep)
          |
          |      val nickOrder: ISZ[Predef.String] = {
          |        val rev: Map[art.Art.BridgeId, String] = Map.empty ++ (for (e <- threadNickNames.entries) yield e._2 ~> e._1)
          |        var ret: ISZ[Predef.String] = ISZ()
          |        for(i <- displayOrder) {
          |          rev.get(i) match {
          |            case Some(k) => ret = ret :+ k.string.native
          |            case _ => ret = ret :+ i.string.native
          |          }
          |        }
          |        ret
          |      }
          |      val cmbThreads = new JComboBox[Predef.String](new java.util.Vector(nickOrder.elements.asJava))
          |      cmbThreads.setSelectedIndex(0)
          |      val btnRunToThread = new JButton("Run To Thread")
          |      btnRunToThread.addActionListener(e => {
          |        val thread = cmbThreads.getSelectedItem.asInstanceOf[Predef.String]
          |        blockingQueue.offer(RunToThread(thread))
          |      })
          |      gadd(leftPanel, btnRunToThread)
          |      gadd(leftPanel, cmbThreads)
          |    }
          |
          |    val rightPanel = new JPanel()
          |
          |    {
          |      val rightLayout = new GridBagLayout()
          |      rightPanel.setLayout(rightLayout)
          |
          |      y = 0
          |      x = 0
          |
          |      gadd(rightPanel, new JLabel("Hyper Period: "))
          |      gadd(rightPanel, jlHyperPeriod)
          |
          |      gadd(rightPanel, new JLabel("Slot: "))
          |      gadd(rightPanel, jlSlot)
          |
          |      //gadd(rightPanel, new JLabel("Domain: "))
          |      //gadd(rightPanel, jlDomain)
          |
          |      gadd(rightPanel, new JLabel("Next Thread: "))
          |      gadd(rightPanel, jlNextThread)
          |
          |    }
          |    val panel = new JPanel(new GridBagLayout())
          |    x = 0
          |    y = 0
          |    gadd(panel, leftPanel)
          |    gadd(panel, rightPanel)
          |
          |    // add some dummy spacers to try and get left and right panels to
          |    // always be in the exactly in the middle
          |    val spacer = new JPanel()
          |    spacer.setPreferredSize(new Dimension(600, 1))
          |    val spacer2 = new JPanel()
          |    spacer2.setPreferredSize(new Dimension(600, 1))
          |    gadd(panel, spacer)
          |    gadd(panel, spacer2)
          |
          |    this.add(panel, BorderLayout.CENTER)
          |
          |    return this
          |  }
          |
          |  override def nextCommand(): Command = {
          |    val s = Explorer.scheduleState
          |    jlHyperPeriod.setText(s.hyperperiodNum.string.native)
          |    jlSlot.setText(s.slotNum.string.native)
          |    jlDomain.setText(Schedule.getDomainFromScheduleState(s).string.native)
          |    jlNextThread.setText(Schedule.getThreadNickNameFromScheduleState(s).native)
          |
          |    val nextHP = s.hyperperiodNum
          |    if (lastHP != nextHP) {
          |      clearColorsCallback()
          |      lastHP = nextHP
          |    }
          |    return blockingQueue.take()
          |  }
          |
          |  override def string: String = toString
          |
          |  override def $$clonable: Boolean = F
          |
          |  override def $$clonable_=(b: Boolean): MutableMarker = this
          |
          |  override def $$owned: Boolean = F
          |
          |  override def $$owned_=(b: Boolean): MutableMarker = this
          |
          |  override def $$clone: MutableMarker = this
          |}
          |
          |class HamrVisionRuntimeMonitor(cp: MOption[CommandProvider]) extends RuntimeMonitorListener {
          |
          |  var visionTreeTable: JTreeTable = _
          |
          |  def clearColors(): Unit = {
          |    visionTreeTable.clearColor()
          |  }
          |
          |  def modelInfoEntry(modelInfo: ModelInfo): ISZ[Entry] = {
          |    val revMap: Map[art.Art.BridgeId, String] = cp match {
          |      case MSome(i: InfoCommandProvider) => Map.empty ++ (for (e <- i.threadNickNames.entries) yield (e._2, e._1))
          |      case _ => Map.empty
          |    }
          |
          |    val orderComponents: ISZ[Component] =
          |      cp match {
          |        case MSome(i: InfoCommandProvider) =>
          |          if (i.displayOrder.nonEmpty) {
          |            val map = Map.empty ++ (for (c <- modelInfo.components) yield (art.Art.BridgeId.fromZ(c.id), c))
          |            val uniqueIds = Set.empty[art.Art.BridgeId]() ++ i.displayOrder
          |            for (id <- uniqueIds.elements) yield map.get(id).get
          |          } else {
          |            modelInfo.components
          |          }
          |        case _ => modelInfo.components
          |      }
          |
          |    var components: ISZ[Entry] = ISZ()
          |
          |    for (c <- orderComponents) {
          |      var inputs: ISZ[Entry] = ISZ()
          |      var outputs: ISZ[Entry] = ISZ()
          |
          |      for (state <- c.state) {
          |        val kind: String = state match {
          |          case i: Port =>
          |            s"$${if (state.direction == StateDirection.In) "Incoming" else "Outgoing"} Port"
          |          case i: StateVariable =>
          |            s"$${if (state.direction == StateDirection.In) "Pre" else "Post"} State Variable"
          |        }
          |        val id = s"$${c.id}_$${state.direction}_$${state.name}"
          |        state.direction match {
          |          case StateDirection.In =>
          |            inputs = inputs :+
          |              hvRow(rowId = id.string, values = ISZ(StringValue(state.name), StringValue(kind), StringValue("")))
          |          case StateDirection.Out =>
          |            outputs = outputs :+
          |              hvRow(rowId = id.string, values = ISZ(StringValue(state.name), StringValue(kind), StringValue("")))
          |        }
          |      }
          |      val componentName: String = revMap.get(art.Art.BridgeId.fromZ(c.id)) match {
          |        case Some(nick) => nick
          |        case _ => c.name
          |      }
          |      var children: ISZ[Entry] = ISZ()
          |      if (inputs.nonEmpty) {
          |        children = children :+ Category(displayName = "Inputs", children = inputs)
          |      }
          |      if (outputs.nonEmpty) {
          |        children = children :+ Category(displayName = "Outputs", children = outputs)
          |      }
          |      components = components :+ Category(displayName = componentName, children = children)
          |    }
          |    return components
          |  }
          |
          |  override def init(modelInfo: ModelInfo): Unit = {
          |    val entries = modelInfoEntry(modelInfo)
          |    visionTreeTable = new JTreeTable(entries, ISZ("State Element", "Description", "Value"))
          |
          |    val visionFrame = new JFrame()
          |    val visionPane = new JScrollPane(visionTreeTable)
          |    visionFrame.add(visionPane, BorderLayout.CENTER)
          |    cp match {
          |      case MSome(i: HamrVisionCommandProvider) =>
          |        visionFrame.add(i, BorderLayout.SOUTH)
          |        i.clearColorsCallback = clearColors _
          |      case MSome(x) => halt(s"Not expecting $$x")
          |      case _ =>
          |    }
          |    visionFrame.setTitle("HAMR Vision Runtime Monitor")
          |    visionFrame.pack()
          |    visionFrame.setSize(new java.awt.Dimension(800, 600))
          |    visionFrame.setVisible(true)
          |  }
          |
          |  def updateOutPorts(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, post: art.DataContent): Unit = {
          |    val m: Map[String, String] = GumboXDispatcher.getUpdates(bridge, observationKind, post)
          |    for (entry <- m.entries) {
          |      visionTreeTable.update(entry._1, ISZ[Option[Value]](None(), None(), Some(StringValue(entry._2))))
          |    }
          |  }
          |
          |  def updateInPorts(bridge: art.Art.BridgeId, observationKind: ObservationKind.Type, pre: art.DataContent): Unit = {
          |    val m: Map[String, String] = GumboXDispatcher.getUpdates(bridge, observationKind, pre)
          |    for (entry <- m.entries) {
          |      visionTreeTable.update(entry._1, ISZ[Option[Value]](None(), None(), Some(StringValue(entry._2))))
          |    }
          |  }
          |
          |  protected implicit val context: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
          |
          |  override def observeInitialisePostState(bridgeId: Art.BridgeId, observationKind: ObservationKind.Type, post: DataContent): Unit = {
          |    Future(updateOutPorts(bridgeId, observationKind, post))
          |  }
          |
          |  override def observeComputePreState(bridgeId: Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[DataContent]): Unit = {
          |    // TODO: really want to run updates in a separate, non-blocking thread. HamrVision should not block
          |    //       the Slang program, nor any other running swing GUI -- I don't think SwingUtilities.invokeLater
          |    //       guarantees the latter
          |    Future(updateInPorts(bridgeId, observationKind, pre.get))
          |  }
          |
          |  override def observeComputePrePostState(bridgeId: Art.BridgeId, observationKind: ObservationKind.Type, pre: Option[DataContent], post: DataContent): Unit = {
          |    Future(updateOutPorts(bridgeId, observationKind, post))
          |  }
          |
          |  override def string: String = toString
          |
          |  override def finalise(): Unit = {}
          |
          |  override def $$clonable: Boolean = F
          |
          |  override def $$clonable_=(b: Boolean): MutableMarker = this
          |
          |  override def $$owned: Boolean = F
          |
          |  override def $$owned_=(b: Boolean): MutableMarker = this
          |
          |  override def $$clone: MutableMarker = this
          |}"""
    return ret
  }
}
