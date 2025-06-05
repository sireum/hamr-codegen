// #Sireum

package org.sireum.hamr.codegen.act.periodic

import org.sireum._
import org.sireum.hamr.codegen.act._
import org.sireum.hamr.codegen.act.proof.ProofContainer.CAmkESConnectionType
import org.sireum.hamr.codegen.act.util.Util.reporter
import org.sireum.hamr.codegen.act.util._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.util.ResourceUtil

@datatype class SelfPacer(val actOptions: ActOptions) extends PeriodicImpl {

  val performHamrIntegration: B = Util.hamrIntegration(actOptions.platform)

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String,
                               symbolTable: SymbolTable): CamkesAssemblyContribution = {

    val threads = symbolTable.getThreads()

    var configurations: ISZ[ast.Configuration] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var auxResources: ISZ[FileResource] = ISZ()

    if (threads.nonEmpty) {
      auxResources = auxResources ++ getSchedule(threads, symbolTable)
    }

    val periodicThreads = symbolTable.getPeriodicThreads()

    for (aadlThread <- periodicThreads) {
      val componentId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)

      configurations = configurations :+ PacerTemplate.domainConfiguration(componentId, aadlThread.getDomain(symbolTable).get)

      val connection = Util.createConnection(
        connectionCategory = CAmkESConnectionType.SelfPacing,
        connectionName = Util.getConnectionName(connectionCounter.increment()),
        connectionType = Sel4ConnectorTypes.seL4Notification,
        srcComponent = componentId,
        srcFeature = SelfPacerTemplate.selfPacerClientTickIdentifier(),
        dstComponent = componentId,
        dstFeature = SelfPacerTemplate.selfPacerClientTockIdentifier()
      )

      connections = connections :+ connection
    }

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)
    val maxDomain: Z = aadlProcessor.getMaxDomain() match {
      case Some(z) => z + 1
      case _ => symbolTable.computeMaxDomain()
    }
    val settingCmakeEntries: ISZ[ST] = ISZ(PacerTemplate.settings_cmake_entries(maxDomain))

    return CamkesAssemblyContribution(
      connections = connections,
      configurations = configurations,
      settingCmakeEntries = settingCmakeEntries,
      auxResourceFiles = auxResources,

      imports = ISZ(),
      instances = ISZ(),
      cContainers = ISZ())
  }

  def handlePeriodicComponent(aadlComponent: AadlComponent, symbolTable: SymbolTable): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    val dispatchableComponent: AadlDispatchableComponent = aadlComponent match {
      case t: AadlThread => t
      case p: AadlProcess => p.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]
      case _ => halt("Unexpected: ")
    }

    assert(dispatchableComponent.isPeriodic())

    val classifier = Util.getClassifier(aadlComponent.component.classifier.get)

    var emits: ISZ[ast.Emits] = ISZ()
    var consumes: ISZ[ast.Consumes] = ISZ()

    var gcHeaderMethods: ISZ[ST] = ISZ()

    var gcMethods: ISZ[ST] = ISZ()
    var gcMainPreLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopStartStms: ISZ[ST] = ISZ()
    var gcMainLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopEndStms: ISZ[ST] = ISZ()

    // initial self pacer/period emit
    gcMainPreLoopStms = gcMainPreLoopStms :+ SelfPacerTemplate.selfPacerEmit()

    // self pacer/period wait at start of loop
    gcMainLoopStartStms = gcMainLoopStartStms :+ SelfPacerTemplate.selfPacerWait()

    // self pacer/period emit at end of loop
    gcMainLoopEndStms = gcMainLoopEndStms :+ SelfPacerTemplate.selfPacerEmit()

    if (!performHamrIntegration && aadlComponent.isInstanceOf[AadlThread]) {
      // get user defined time triggered method
      val t = aadlComponent.asInstanceOf[AadlThread]
      t.getComputeEntrypointSourceText() match {
        case Some(handler) =>
          // header method so developer knows required signature
          gcHeaderMethods = gcHeaderMethods :+ st"void ${handler}(const int64_t * in_arg);"

          gcMethods = gcMethods :+ SelfPacerTemplate.wrapPeriodicComputeEntrypoint(classifier, handler)

          gcMainLoopStms = gcMainLoopStms :+ SelfPacerTemplate.callPeriodicComputEntrypoint(classifier, handler)

        case _ =>
          reporter.warn(None(), Util.toolName, s"Periodic thread ${classifier} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
      }
    }

    emits = emits :+ Util.createEmits_SelfPacing(
      aadlComponent = aadlComponent,
      symbolTable = symbolTable,
      name = SelfPacerTemplate.selfPacerClientTickIdentifier(),
      typ = SelfPacerTemplate.selfPacerTickTockType())

    consumes = consumes :+ Util.createConsumes_SelfPacing(
      aadlComponent = aadlComponent,
      symbolTable = symbolTable,
      name = SelfPacerTemplate.selfPacerClientTockIdentifier(),
      typ = SelfPacerTemplate.selfPacerTickTockType(),
      optional = F)

    val shell = ast.Component(
      emits = emits,
      consumes = consumes,

      // filler
      control = F, hardware = F, name = "",
      dataports = ISZ(), includes = ISZ(), mutexes = ISZ(), binarySemaphores = ISZ(), semaphores = ISZ(),
      imports = ISZ(), uses = ISZ(), provides = ISZ(), attributes = ISZ(),
      preprocessorIncludes = ISZ(), externalEntities = ISZ(),
      comments = ISZ()
    )

    val componentContributions = CamkesComponentContributions(shell)

    val glueCodeContributions = CamkesGlueCodeContributions(
      CamkesGlueCodeHeaderContributions(includes = ISZ(), methods = gcHeaderMethods),
      CamkesGlueCodeImplContributions(includes = ISZ(), globals = ISZ(), methods = gcMethods, preInitStatements = ISZ(),
        postInitStatements = ISZ(),
        mainPreLoopStatements = gcMainPreLoopStms,
        mainLoopStartStatements = gcMainLoopStartStms,
        mainLoopStatements = gcMainLoopStms,
        mainLoopEndStatements = gcMainLoopEndStms,
        mainPostLoopStatements = ISZ()
      )
    )

    return (componentContributions, glueCodeContributions)
  }

  def getSchedule(allThreads: ISZ[AadlThread], symbolTable: SymbolTable): ISZ[FileResource] = {

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)

    val path = "kernel/domain_schedule.c"

    val contents: ST = aadlProcessor.getScheduleSourceText() match {
      case Some(path2) =>
        if (Os.path(path2).exists) {
          val p = Os.path(path2)
          st"${p.read}"
        } else {
          actOptions.workspaceRootDir match {
            case Some(root) =>
              val candidate = Os.path(root) / path2
              if (candidate.exists) {
                st"${candidate.read}"
              } else {
                halt(s"Could not locate Schedule_Source_Text ${candidate}")
              }
            case _ => halt(s"Unexpected: Couldn't locate Schedule_Source_Text ${path2}")
          }
        }
      case _ =>
        var entries: ISZ[ST] = ISZ()

        val clockPeriod: Z = aadlProcessor.getClockPeriod() match {
          case Some(z) => z
          case _ => halt("Unexpected: Clock_Period not specified")
        }

        val framePeriod: Z = aadlProcessor.getFramePeriod() match {
          case Some(z) => z
          case _ => halt("Unexpected: Frame_Period not specified")
        }

        val otherLen = z"200"
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", otherLen / clockPeriod,
          Some(st" // all other seL4 threads, init, ${otherLen}ms"))

        val domainZeroLen: Z = z"10"
        val domainZeroEntry = PacerTemplate.pacerScheduleEntry(z"0", domainZeroLen / clockPeriod,
          Some(st" // switch to domain 0 to allow seL4 to deliver messages"))

        var threadComments: ISZ[ST] = ISZ()
        var sumExecutionTime = z"0"
        for (index <- 0 until allThreads.size) {
          val p = allThreads(index)
          val threadName = Util.getCamkesComponentIdentifier(p, symbolTable)

          val domain = p.getDomain(symbolTable).get
          val computeExecutionTime = p.getMaxComputeExecutionTime()
          val comment = Some(st" // ${threadName} ${computeExecutionTime} ms")

          threadComments = threadComments :+
            PacerTemplate.pacerScheduleThreadPropertyComment(threadName, "Thread",
              domain, p.dispatchProtocol, s"${computeExecutionTime} ms", p.period)

          entries = entries :+ PacerTemplate.pacerScheduleEntry(domain, computeExecutionTime / clockPeriod, comment)

          sumExecutionTime = sumExecutionTime + computeExecutionTime

          if (index < allThreads.size - 1) {
            entries = entries :+ domainZeroEntry
            sumExecutionTime = sumExecutionTime + domainZeroLen
          }
        }

        val pad: Z = (framePeriod - (otherLen + sumExecutionTime)) / clockPeriod
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", pad, Some(st" // pad rest of frame period"))

        PacerTemplate.pacerExampleSchedule(clockPeriod, framePeriod, threadComments, entries, F)
    }

    return ISZ(ResourceUtil.createResource(path, contents, F))
  }
}
