// #Sireum

package org.sireum.hamr.codegen.act.periodic

import org.sireum._
import org.sireum.hamr.codegen.act._
import org.sireum.hamr.codegen.act.proof.ProofContainer.CAmkESConnectionType
import org.sireum.hamr.codegen.act.util.Util.reporter
import org.sireum.hamr.codegen.act.util._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.properties.OsateProperties
import org.sireum.hamr.codegen.common.util.{ResourceUtil, TimeUtil}

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
    var userContributions: ISZ[FileResource] = ISZ()

    if (threads.nonEmpty) {
      val f = getSchedule(threads, symbolTable)
      if (f._1) {
        userContributions = userContributions :+ f._2
      } else {
        auxResources = auxResources :+ f._2
      }
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
      userContributions = userContributions,

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

  def getSchedule(allThreads: ISZ[AadlThread], symbolTable: SymbolTable): (B, FileResource) = {

    val aadlProcessor = PeriodicUtil.getBoundProcessor(symbolTable)

    val path = "kernel/domain_schedule.c"

    val (contents, userSupplied): (ST, B) = aadlProcessor.getScheduleSourceText() match {
      case Some(path2) =>
        if (Os.path(path2).exists) {
          val p = Os.path(path2)
          (st"${p.read}", T)
        } else {
          actOptions.workspaceRootDir match {
            case Some(root) =>
              val candidate = Os.path(root) / path2
              if (candidate.exists) {
                (st"${candidate.read}", T)
              } else {
                halt(s"Could not locate Schedule_Source_Text ${candidate}")
              }
            case _ => halt(s"Unexpected: Couldn't locate Schedule_Source_Text ${path2}")
          }
        }
      case _ =>
        var entries: ISZ[ST] = ISZ()

        // all lengths below are in Clock_Period ticks (PacerTimeUtil)
        val clockPeriodPs: Z = PacerTimeUtil.clockPeriodPs(aadlProcessor, reporter)
        val framePeriodPs: Z = PacerTimeUtil.framePeriodPs(aadlProcessor)
        val frameTicks: Z = PacerTimeUtil.toTicks(framePeriodPs, clockPeriodPs,
          PacerTimeUtil.processorWhat(aadlProcessor, "Frame_Period", OsateProperties.TIMING_PROPERTIES__FRAME_PERIOD),
          aadlProcessor.component.identifier.pos, reporter)

        val otherTicks = PacerTimeUtil.fixedTicks(PacerTimeUtil.otherLenPs, clockPeriodPs, "all other seL4 threads and init", reporter)
        entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", otherTicks,
          Some(st" // all other seL4 threads, init, ${TimeUtil.format(otherTicks * clockPeriodPs)}"))

        val domainZeroTicks: Z = PacerTimeUtil.fixedTicks(PacerTimeUtil.domainZeroLenPs, clockPeriodPs, "domain 0 between components", reporter)
        val domainZeroEntry = PacerTemplate.pacerScheduleEntry(z"0", domainZeroTicks,
          Some(st" // switch to domain 0 to allow seL4 to deliver messages"))

        var threadComments: ISZ[ST] = ISZ()
        var usedTicks: Z = otherTicks
        for (index <- 0 until allThreads.size) {
          val p = allThreads(index)
          val threadName = Util.getCamkesComponentIdentifier(p, symbolTable)

          val domain = p.getDomain(symbolTable).get
          val computeExecutionTimePs = PacerTimeUtil.threadComputeExecutionTimePs(p, reporter)
          val ticks = PacerTimeUtil.toTicks(computeExecutionTimePs, clockPeriodPs, PacerTimeUtil.cetWhat(p),
            p.component.identifier.pos, reporter)
          val comment = Some(st" // ${threadName} ${TimeUtil.format(ticks * clockPeriodPs)}")
          val origin: String = if (p.computeExecutionTimePs.isEmpty) " (default)" else ""

          threadComments = threadComments :+
            PacerTemplate.pacerScheduleThreadPropertyComment(threadName, "Thread",
              domain, p.dispatchProtocol, s"${TimeUtil.format(computeExecutionTimePs)}$origin", p.periodPs)

          entries = entries :+ PacerTemplate.pacerScheduleEntry(domain, ticks, comment)

          usedTicks = usedTicks + ticks

          if (index < allThreads.size - 1) {
            entries = entries :+ domainZeroEntry
            usedTicks = usedTicks + domainZeroTicks
          }
        }

        // a zero pad is omitted: seL4 cannot run a zero-length domain entry safely
        val pad: Z = PacerTimeUtil.padTicks(frameTicks, usedTicks, clockPeriodPs, aadlProcessor, reporter)
        if (pad > 0) {
          entries = entries :+ PacerTemplate.pacerScheduleEntry(z"0", pad, Some(st" // pad rest of frame period"))
        }

        (PacerTemplate.pacerExampleSchedule(clockPeriodPs, framePeriodPs, threadComments, entries, F), F)
    }

    if (userSupplied) {
      return (T, ResourceUtil.createResourceI(
        path = path, content = contents, overwrite = F, isDatatype = F, skipConsistencyChecks = T))
    } else {
      return (F, ResourceUtil.createResource(path, contents, F))
    }
  }
}
