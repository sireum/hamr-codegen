// #Sireum

package org.sireum.hamr.codegen.act.periodic

import org.sireum._
import org.sireum.hamr.codegen.act._
import org.sireum.hamr.codegen.act.ast._
import org.sireum.hamr.codegen.act.periodic.PeriodicDispatcherTemplate._
import org.sireum.hamr.codegen.act.proof.ProofContainer.{CAmkESComponentCategory, CAmkESConnectionType}
import org.sireum.hamr.codegen.act.templates.StringTemplate
import org.sireum.hamr.codegen.act.util.Util.reporter
import org.sireum.hamr.codegen.act.util._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.properties.PropertyUtil
import org.sireum.hamr.codegen.common.symbols._

@datatype class PeriodicDispatcher(val actOptions: ActOptions) extends PeriodicImpl {

  val performHamrIntegration: B = Util.hamrIntegration(actOptions.platform)

  val hookupPeriodicComponentsToTimeServer: B = F

  def handlePeriodicComponents(connectionCounter: Counter,
                               timerAttributeCounter: Counter,
                               headerInclude: String,
                               symbolTable: SymbolTable): CamkesAssemblyContribution = {

    val components: ISZ[AadlComponent] = symbolTable.componentMap.values

    var imports: ISZ[String] = ISZ()
    var instances: ISZ[ast.Instance] = ISZ()
    var connections: ISZ[ast.Connection] = ISZ()
    var configurations: ISZ[ast.Configuration] = ISZ()
    var cContainers: ISZ[C_Container] = ISZ()
    var auxResources: ISZ[FileResource] = ISZ()

    val periodicComponents: ISZ[AadlThread] = symbolTable.getThreads().filter(c => CommonUtil.isPeriodic(c)).map(m => m)

    if (periodicComponents.nonEmpty) {
      var periodicDispatcherNotifications: ISZ[ast.Emits] = ISZ()
      var periodicDispatcherCalendars: ISZ[ST] = ISZ()

      for (aadlThread <- periodicComponents) {
        val camkesComponentId = Util.getCamkesComponentIdentifier(aadlThread, symbolTable)
        val classifier = Util.getClassifier(aadlThread.component.classifier.get)

        if (hookupPeriodicComponentsToTimeServer) {
          // connect camkes component to time server
          connections = connections :+ Util.createConnection(
            CAmkESConnectionType.PeriodicDispatching,
            Util.getConnectionName(connectionCounter.increment()),
            Sel4ConnectorTypes.seL4TimeServer,
            camkesComponentId, PeriodicDispatcherTemplate.TIMER_ID,
            PeriodicDispatcherTemplate.TIMER_INSTANCE,
            PeriodicDispatcherTemplate.TIMER_SERVER_TIMER_ID)

          // timer attribute
          configurations = configurations :+
            PeriodicDispatcherTemplate.configurationTimerAttribute(camkesComponentId, timerAttributeCounter.increment(), F)
        }

        val componentNotificationName = PeriodicDispatcherTemplate.componentNotificationName(None())
        val dispatcherNotificationName = PeriodicDispatcherTemplate.componentNotificationName(Some(camkesComponentId))

        // emit notification when component's period occurs
        periodicDispatcherNotifications = periodicDispatcherNotifications :+ Emits(
          name = dispatcherNotificationName,
          typ = Util.NOTIFICATION_TYPE,
          comments = ISZ())

        // connect dispatcher to component
        connections = connections :+ Util.createConnection(
          CAmkESConnectionType.PeriodicDispatching,
          Util.getConnectionName(connectionCounter.increment()),
          Sel4ConnectorTypes.seL4Notification,
          PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE, dispatcherNotificationName,
          camkesComponentId, componentNotificationName)

        val period: Z = PropertyUtil.getPeriod(aadlThread.component) match {
          case Some(_period) => _period
          case _ =>
            reporter.warn(None(), Util.toolName, s"Period not provided for periodic component ${classifier}, using ${Util.DEFAULT_PERIOD}")
            Util.DEFAULT_PERIOD
        }

        periodicDispatcherCalendars = periodicDispatcherCalendars :+ PeriodicDispatcherTemplate.calendar(camkesComponentId, period)
      }

      // add the dispatcher component

      val dispatchCamkesComponent: ast.Instance = genDispatchCamkesComponent(periodicDispatcherNotifications)

      instances = instances :+ dispatchCamkesComponent
      instances = instances :+ timerComponent()

      val dispatchCSource = PeriodicDispatcherTemplate.dispatchComponentCSource(
        headerInclude, periodicDispatcherCalendars)

      cContainers = cContainers :+ C_Container(
        instanceName = dispatchCamkesComponent.component.name,
        componentId = dispatchCamkesComponent.component.name,
        cSources = ISZ(dispatchCSource),
        cIncludes = ISZ(),
        sourceText = ISZ(),
        cmakeSOURCES = ISZ(),
        cmakeINCLUDES = ISZ(),
        cmakeLIBS = ISZ(Util.SBTypeLibrary)
      )

      // connect dispatch timer to time server
      connections = connections :+ Util.createConnection(
        CAmkESConnectionType.PeriodicDispatching,
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4TimeServer,
        PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE, PeriodicDispatcherTemplate.TIMER_ID_DISPATCHER,
        PeriodicDispatcherTemplate.TIMER_INSTANCE, PeriodicDispatcherTemplate.TIMER_SERVER_TIMER_ID)

      // connect notification/callback from time server to dispatch timer
      connections = connections :+ Util.createConnection(
        CAmkESConnectionType.PeriodicDispatching,
        Util.getConnectionName(connectionCounter.increment()),
        Sel4ConnectorTypes.seL4GlobalAsynchCallback,
        PeriodicDispatcherTemplate.TIMER_INSTANCE, PeriodicDispatcherTemplate.TIMER_SERVER_NOTIFICATION_ID,
        PeriodicDispatcherTemplate.DISPATCH_PERIODIC_INSTANCE, PeriodicDispatcherTemplate.TIMER_NOTIFICATION_DISPATCHER_ID)

      configurations = configurations :+ ast.GenericConfiguration(s"${PeriodicDispatcherTemplate.TIMER_INSTANCE}.timers_per_client = 1;", ISZ())

      configurations = configurations :+ PeriodicDispatcherTemplate.configurationTimerAttribute(dispatchCamkesComponent.name,
        timerAttributeCounter.increment(), T)

      configurations = configurations :+ StringTemplate.configurationPriority(dispatchCamkesComponent.name, Util.DEFAULT_PRIORITY)

      imports = imports ++ ISZ(Util.camkesStdConnectors, Util.camkesGlobalConnectors, PeriodicDispatcherTemplate.TIMER_SERVER_IMPORT)
    }

    val settingsCmakeEntries: ISZ[ST] = ISZ()
    return CamkesAssemblyContribution(imports, instances, connections, configurations, cContainers,
      settingsCmakeEntries, auxResources)
  }


  def handlePeriodicComponent(aadlComponent: AadlComponent, symbolTable: SymbolTable): (CamkesComponentContributions, CamkesGlueCodeContributions) = {
    val component = aadlComponent.component
    val classifier = Util.getClassifier(component.classifier.get)

    var imports: ISZ[String] = ISZ()
    var uses: ISZ[ast.Uses] = ISZ()
    var consumes: ISZ[ast.Consumes] = ISZ()

    var gcHeaderMethods: ISZ[ST] = ISZ()

    var gcMethods: ISZ[ST] = ISZ()

    var gcMainPreInitStatements: ISZ[ST] = ISZ()

    var gcMainPreLoopStms: ISZ[ST] = ISZ()
    var gcMainLoopStms: ISZ[ST] = ISZ()

    // import Timer.idl4
    imports = imports :+ Util.camkesStdConnectors

    if (hookupPeriodicComponentsToTimeServer) {
      // uses Timer tb_timer;
      uses = uses :+ Util.createUses_PeriodicDispatcher(
        aadlComponent = aadlComponent,
        name = PeriodicDispatcherTemplate.TIMER_ID,
        typ = PeriodicDispatcherTemplate.TIMER_TYPE,
        optional = F)
    }

    // consumes Notification from periodic dispatcher
    consumes = consumes :+ Util.createConsumes_PeriodicDispatcher(
      aadlComponent = aadlComponent,
      name = PeriodicDispatcherTemplate.componentNotificationName(None()),
      typ = Util.NOTIFICATION_TYPE,
      optional = F)

    gcMethods = gcMethods :+ PeriodicDispatcherTemplate.periodicDispatchElems(hookupPeriodicComponentsToTimeServer)

    gcMainPreInitStatements = gcMainPreInitStatements :+ PeriodicDispatcherTemplate.registerPeriodicCallback()

    if (!performHamrIntegration && aadlComponent.isInstanceOf[AadlThread]) {
      val t = aadlComponent.asInstanceOf[AadlThread]
      t.getComputeEntrypointSourceText() match {
        case Some(handler) =>
          gcHeaderMethods = gcHeaderMethods :+ st"void ${handler}(const int64_t *);"

          val drains = PeriodicDispatcherTemplate.drainPeriodicQueue(classifier, handler)

          gcMethods = gcMethods :+ drains._1

          gcMainLoopStms = gcMainLoopStms :+ drains._2

        case _ =>
          reporter.warn(None(), Util.toolName, s"Periodic thread ${classifier} is missing property ${Util.PROP_TB_SYS__COMPUTE_ENTRYPOINT_SOURCE_TEXT} and will not be dispatched")
      }
    }

    val shell = ast.Component(
      imports = imports,
      uses = uses,
      consumes = consumes,

      // filler
      control = F, hardware = F, name = "", mutexes = ISZ(), binarySemaphores = ISZ(), semaphores = ISZ(),
      dataports = ISZ(), emits = ISZ(), provides = ISZ(), includes = ISZ(), attributes = ISZ(),
      preprocessorIncludes = ISZ(), externalEntities = ISZ(),
      comments = ISZ()
    )
    val componentContributions = CamkesComponentContributions(shell)

    val glueCodeContributions = CamkesGlueCodeContributions(
      CamkesGlueCodeHeaderContributions(includes = ISZ(), methods = gcHeaderMethods),
      CamkesGlueCodeImplContributions(includes = ISZ(), globals = ISZ(), methods = gcMethods,
        preInitStatements = gcMainPreInitStatements,
        postInitStatements = ISZ(),

        mainPreLoopStatements = gcMainPreLoopStms,
        mainLoopStartStatements = ISZ(),
        mainLoopStatements = gcMainLoopStms,
        mainLoopEndStatements = ISZ(),
        mainPostLoopStatements = ISZ())
    )

    return (componentContributions, glueCodeContributions)
  }

  def genDispatchCamkesComponent(notifications: ISZ[ast.Emits]): ast.Instance = {
    val i = Util.createCAmkESInstance(
      originAadl = None(),

      address_space = "",
      name = DISPATCH_PERIODIC_INSTANCE,
      component = Util.createCAmkESComponent(
        aadlThread = None(),
        componentCategory = CAmkESComponentCategory.PeriodicDispatcher,
        control = T,
        hardware = F,
        name = DISPATCH_CLASSIFIER,
        mutexes = ISZ(),
        binarySemaphores = ISZ(),
        semaphores = ISZ(),
        dataports = ISZ(),
        emits = notifications,
        uses = ISZ(ast.Uses(
          name = DISPATCH_TIMER_ID,
          typ = TIMER_TYPE,
          optional = F,
          comments = ISZ())),
        consumes = ISZ(ast.Consumes(
          name = TIMER_NOTIFICATION_DISPATCHER_ID,
          typ = Util.NOTIFICATION_TYPE,
          optional = F,
          comments = ISZ())),
        provides = ISZ(),
        includes = ISZ(),
        attributes = ISZ(),
        preprocessorIncludes = ISZ(),
        imports = ISZ(Util.camkesGlobalConnectors),
        externalEntities = ISZ()
      ),
      comments = ISZ()
    )
    return i
  }

  def timerComponent(): ast.Instance = {
    val i = Util.createCAmkESInstance(
      originAadl = None(),

      address_space = "",
      name = TIMER_INSTANCE,
      component = Util.createCAmkESLibraryComponent(
        componentCategory = CAmkESComponentCategory.TimeServer,

        name = TIMER_SERVER_CLASSIFIER,
        ports = ISZ(TIMER_SERVER_NOTIFICATION_ID, TIMER_SERVER_TIMER_ID)
      ),
      comments = ISZ()
    )
    return i
  }


}
