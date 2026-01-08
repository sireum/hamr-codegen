// #Sireum

package org.sireum.hamr.codegen.arsit

import org.sireum._
import org.sireum.hamr.codegen.arsit.Util.{nameProvider, toolName}
import org.sireum.hamr.codegen.arsit.nix.NixGen
import org.sireum.hamr.codegen.arsit.plugin.DatatypeProviderPlugin
import org.sireum.hamr.codegen.arsit.templates._
import org.sireum.hamr.codegen.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.codegen.arsit.util.{ArsitOptions, SchedulerUtil}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.containers._
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.resolvers.GclResolver
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{ConnectionInstance, GclMethod}
import org.sireum.message.{Position, Reporter}
import org.sireum.ops.ISZOps

@record class ArchitectureGenerator(val directories: ProjectDirectories,
                                    val rootSystem: AadlSystem,
                                    val arsitOptions: ArsitOptions,
                                    val symbolTable: SymbolTable,
                                    val types: AadlTypes) {
  var componentId: Z = 0
  var portId: Z = 0

  val basePackage: String = arsitOptions.packageName
  var bridges: ISZ[ST] = ISZ()
  var components: ISZ[String] = ISZ[String]()
  var connections: ISZ[ST] = ISZ()

  var seenConnections: HashMap[ir.Name, ISZ[ir.Name]] = HashMap.empty

  var resources: ISZ[Resource] = ISZ()

  def generate(plugins: ISZ[Plugin], store: Store): (Result, Store) = {
    var localStore = store
    if (!types.rawConnections) {
      // TODO allow for customizations of base types
      for (aadlType <- getTouchedTypes(types, symbolTable, reporter) if !aadlType.isInstanceOf[BaseType]) {
        val defaultTemplate: IDatatypeTemplate = {
          if (aadlType.isInstanceOf[BaseType]) {
            halt(s"Support for customizing base type ${aadlType.name} hasn't been implemented yet")
          }

          aadlType match {
            case e: EnumType => EnumTemplate(e, e.values, T)
            case e: RecordType => DatatypeTemplate(e, GclResolver.getIndexingTypeFingerprints(localStore), T)
            case e: ArrayType => DatatypeTemplate(e, GclResolver.getIndexingTypeFingerprints(localStore), T)
            case e: TODOType => DatatypeTemplate(e, GclResolver.getIndexingTypeFingerprints(localStore), F)
          }
        }

        val resolvedAnnexSubclauses: ISZ[AnnexClauseInfo] =
          symbolTable.annexClauseInfos.get(ISZ(aadlType.name)) match {
            case Some(annexInfos) => annexInfos
            case _ => ISZ()
          }

        var fulls: ISZ[(String, DatatypeProviderPlugin.FullDatatypeContribution)] = ISZ()
        var partials: ISZ[(String, DatatypeProviderPlugin.PartialDatatypeContribution)] = ISZ()
        for (p <- plugins if p.isInstanceOf[DatatypeProviderPlugin] &&
          p.asInstanceOf[DatatypeProviderPlugin].canHandleDatatypeProvider(
            aadlType,
            resolvedAnnexSubclauses,
            types,
            symbolTable, localStore)) {

          p.asInstanceOf[DatatypeProviderPlugin].handleDatatypeProvider(
            arsitOptions.packageName,
            aadlType,
            defaultTemplate,
            resolvedAnnexSubclauses,
            aadlType.nameProvider.filePath,
            directories, types, symbolTable, localStore, reporter) match {
            case (f: DatatypeProviderPlugin.FullDatatypeContribution, s) =>
              fulls = fulls :+ (p.name, f)
              localStore = s
            case (pc: DatatypeProviderPlugin.PartialDatatypeContribution, s) =>
              partials = partials :+ (p.name, pc)
              localStore = s
          }
        }

        if (fulls.nonEmpty) {
          if (fulls.size != 1) {
            reporter.error(aadlType.container.get.identifier.pos, toolName,
              st"Only one FullDatatypeContribution can be made but the following plugins all made such contributions: ${(for (f <- fulls) yield f._1, ", ")})".render)
          } else if (partials.nonEmpty) {
            reporter.error(aadlType.container.get.identifier.pos, toolName,
              st"Cannot mix full and partial datatype contributions. ${fulls(0)._1} is providing a FullDatatypeContribution whereas the following plugins are providing partial ones: ${(for (p <- partials) yield p._1)}".render)
          } else {
            resources = (resources :+ fulls(0)._2.datatype) ++ fulls(0)._2.resources
          }
        } else {
          var pcs = DatatypeProviderPlugin.emptyPartialDatatypeContributions
          for (p <- partials) {
            pcs = pcs(
              slangSwitches = pcs.slangSwitches ++ p._2.slangSwitches,
              imports = pcs.imports ++ p._2.imports,
              datatypeSingletonBlocks = pcs.datatypeSingletonBlocks ++ p._2.datatypeSingletonBlocks,
              datatypeBlocks = pcs.datatypeBlocks ++ p._2.datatypeBlocks,
              payloadSingletonBlocks = pcs.payloadSingletonBlocks ++ p._2.payloadSingletonBlocks,
              preBlocks = pcs.preBlocks ++ p._2.preBlocks,
              postBlocks = pcs.postBlocks ++ p._2.postBlocks,
              resources = pcs.resources ++ p._2.resources)
          }
          val content: ST = defaultTemplate match {
            case e: EnumTemplate =>
              // TODO would partial providers ever contribute to enums?
              e.generateDefault()
            case d: DatatypeTemplate =>
              d.generateDefault(
                additionalSwitches = pcs.slangSwitches,
                additionalImports = pcs.imports,
                additionalDatatypeCompanionBlocks = pcs.datatypeSingletonBlocks,
                additionParams = ISZ(), // hmm not sure if partial providers would add params
                additionalDatatypeBlocks = pcs.datatypeBlocks,
                additionalPayloadSingletonBlocks = pcs.payloadSingletonBlocks,
                additionalPreBlocks = pcs.preBlocks,
                addtionalPostBlocks = pcs.postBlocks)
          }
          // To ponder: why would a datatype plugin provider only supply partial
          // contributions for TODOTypes?  Those are specifically the cases where an
          // external plugin should be stepping in and supplying the full implementation
          val shouldOverwrite = !aadlType.isInstanceOf[TODOType]

          resources = (resources :+ ResourceUtil.createResourceH(
            path = s"${directories.dataDir}/${aadlType.nameProvider.filePath}",
            content = content, overwrite = shouldOverwrite, isDatatype = T)) ++ pcs.resources
        }
      }
    }

    val baseTypes = TypeTemplate.Base_Types(basePackage)
    resources = resources :+ ResourceUtil.createResourceH(Util.pathAppend(directories.dataDir, ISZ(basePackage, "Base_Types.scala")), baseTypes, T, T)

    generateInternal()

    return (
      ArsitResult(
        resources = resources,
        maxPort = portId,
        maxComponent = componentId,
        maxConnection = connections.size),
      localStore)
  }

  @pure def getTouchedTypes(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): ISZ[AadlType] = {
    var ret: Set[AadlType] = Set.empty

    def add(posOpt: Option[Position], aadlType: AadlType): Unit = {
      aadlType match {
        case t: ArrayType =>
          add(posOpt, t.baseType)
        case t: RecordType =>
          for (f <- t.fields.values) {
            add(posOpt, f)
          }
        case _ =>
      }

      ret = ret + aadlType
    }

    @pure def processType(typed: org.sireum.lang.ast.Typed.Name, posOpt: Option[Position]): Unit = {
      val name = TypeUtil.getAadlTypeFromSlangType(typed.ids)
      add(posOpt, aadlTypes.typeMap.get(name).get)
    }

    def processMethod(m: GclMethod): Unit = {
      m.method.sig.returnType.typedOpt match {
        case Some(t: org.sireum.lang.ast.Typed.Name) => processType(t, m.posOpt)
        case _ =>
      }
      for (p <- m.method.sig.params) {
        p.tipe.typedOpt match {
          case Some(typed: org.sireum.lang.ast.Typed.Name) => processType(typed, p.id.attr.posOpt)
          case _ =>
        }
      }
    }

    for (thread <- if (arsitOptions.devicesAsThreads) symbolTable.getThreadOrDevices() else symbolTable.getThreads();
         port <- thread.getPorts()) {
      port match {
        case d: AadlFeatureData => add(port.feature.identifier.pos, d.aadlType)
        case _ =>
      }
    }

    for (annexes <- symbolTable.annexClauseInfos.values;
         a <- annexes) {
      a match {
        case g: GclAnnexClauseInfo =>
          for (s <- g.annex.state) {
            add(s.posOpt, aadlTypes.typeMap.get(s.classifier).get)
          }
          for (m <- g.annex.methods) {
            processMethod(m)
          }
      }
    }

    for (gclLib <- symbolTable.annexLibInfos) {
      gclLib match {
        case lib: GclAnnexLibInfo =>
          for (m <- lib.annex.methods) {
            processMethod(m)
          }
        case _ =>
      }
    }

    return ret.elements
  }

  def generateInternal(): Unit = {
    gen(rootSystem)

    val architectureName = "Arch"
    val architectureDescriptionName = "ad"

    val touchMethod: Option[ST] =
      if (NixGen.willTranspile(arsitOptions.platform)) {
        val components: ISZ[AadlThreadOrDevice] =
          if (arsitOptions.devicesAsThreads) symbolTable.getThreadOrDevices()
          else symbolTable.getThreads().map(m => m.asInstanceOf[AadlThreadOrDevice])

        val typeTouches = NixGen.genTypeTouches(types)
        val apiTouches = NixGen.genApiTouches(types, basePackage, components)
        val scheduleTouches = SchedulerUtil.getSchedulerTouches(symbolTable, arsitOptions.devicesAsThreads)
        Some(SeL4NixTemplate.genTouchMethod(typeTouches, apiTouches, scheduleTouches))
      }
      else {
        None()
      }

    val arch = ArchitectureTemplate.architectureDescription(
      packageName = basePackage,
      imports = ISZ(),
      architectureName = architectureName,
      architectureDescriptionName = architectureDescriptionName,
      bridges = bridges,
      components = components,
      connections = connections,
      touchMethod = touchMethod
    )

    addResource(directories.architectureDir, ISZ(basePackage, "Arch.scala"), arch, T)

    val demo = ArchitectureTemplate.demo(basePackage, architectureName, architectureDescriptionName)
    addResource(directories.architectureDir, ISZ(basePackage, "Demo.scala"), demo, F)

    val schedulers = SchedulerTemplate.schedulers(basePackage, components,
      SchedulerUtil.getProcessorTimingProperties(symbolTable),
      SchedulerUtil.getThreadTimingProperties(symbolTable, arsitOptions.devicesAsThreads),
      SchedulerUtil.getFramePeriod(symbolTable))
    addResource(directories.architectureDir, ISZ(basePackage, "Schedulers.scala"), schedulers, T)

    val scheduleProvider = SchedulerTemplate.scheduleProvider(basePackage)
    addResource(directories.architectureDir, ISZ(basePackage, "ScheduleProvider.scala"), scheduleProvider, T)

    val inspectorDemo = InspectorTemplate.inspectorDemo(basePackage)
    addResource(directories.inspectorDir, ISZ(basePackage, "InspectorDemo.scala"), inspectorDemo, T)
  }

  def getComponentId(): Z = {
    val id = componentId
    componentId = componentId + 1
    return id
  }

  def gen(c: AadlComponent): Unit = {
    c match {
      case s: AadlSystem => genContainer(s)
      case s: AadlProcess => genContainer(s)

      case s: AadlThreadGroup => genThreadGroup(s)

      case _ =>
        for (_c <- c.subComponents) {
          gen(_c)
        }
    }
  }

  def genContainer(m: AadlComponent): Unit = {
    assert(m.isInstanceOf[AadlSystem] || m.isInstanceOf[AadlProcess])

    for (c <- m.subComponents) {
      c match {
        case s: AadlSystem => genContainer(s)
        case s: AadlProcess => genContainer(s)

        case s: AadlThreadGroup => genThreadGroup(s)

        case s: AadlThread =>
          val names = nameProvider(s.component, basePackage)
          bridges = bridges :+ genThread(s, names)
          components = components :+ names.instanceName

        case s: AadlDevice =>
          if (arsitOptions.devicesAsThreads) {
            val names = nameProvider(s.component, basePackage)
            bridges = bridges :+ genThread(s, names)
            components = components :+ names.instanceName
          }

        case s: AadlSubprogram => // not relevant for arch

        case x =>
          reporter.info(None(), Util.toolName, s"Skipping: ${x.component.category} component ${CommonUtil.getName(x.component.identifier)}")
      }
    }

    connections = connections ++ m.connectionInstances
      .filter((ci: ir.ConnectionInstance) => allowConnection(ci, m.component))
      .map((ci: ir.ConnectionInstance) => processConnectionInstance(ci))
  }


  def genThreadGroup(m: AadlThreadGroup): Unit = {

    for (c <- m.subComponents) {
      assert(c.isInstanceOf[AadlThread])
      val names = nameProvider(c.component, basePackage)
      bridges = bridges :+ genThread(c.asInstanceOf[AadlThread], names)
      components = components :+ names.instanceName
    }

    connections = connections ++ m.connectionInstances
      .filter((ci: ir.ConnectionInstance) => allowConnection(ci, m.component))
      .map((ci: ir.ConnectionInstance) => processConnectionInstance(ci))
  }

  def genThread(m: AadlThreadOrDevice, names: NameProvider): ST = {
    assert(!m.isInstanceOf[AadlDevice] || arsitOptions.devicesAsThreads)

    assert(m.connectionInstances.isEmpty)

    val id = getComponentId()

    val period: Z = CommonUtil.getPeriod(m)

    val dispatchProtocol: Dispatch_Protocol.Type = m.dispatchProtocol

    val dispatchProtocolST: ST = ArchitectureTemplate.dispatchProtocol(dispatchProtocol, period)

    val dispatchTriggers: Option[ISZ[String]] = Util.getDispatchTriggers(m.component)

    val ports: ISZ[Port] = Util.getPorts(m, types, basePackage, portId)
    portId = portId + ports.size

    val _ports = ports.map((p: Port) => ArchitectureTemplate.genPort(p))
    val _portArgs = ports.map((p: Port) => st"${p.name} = ${p.name}")

    return ArchitectureTemplate.bridge(
      bridgeIdentifier = names.instanceName,
      instanceName = names.instanceName,
      typeName = names.bridgeTypeName,
      id = id,
      dispatchProtocol = dispatchProtocolST,
      dispatchTriggers = dispatchTriggers,
      ports = _ports,
      portArguments = _portArgs)
  }

  def processConnectionInstance(ci: ConnectionInstance): ST = {
    val srcComponentId = CommonUtil.getName(ci.src.component)
    val srcFeatureId = ci.src.feature.get.name
    val srcComponentFeatureId = symbolTable.featureMap.get(srcFeatureId).get.identifier

    val dstComponentId = CommonUtil.getName(ci.dst.component)
    val dstFeatureId = ci.dst.feature.get.name
    val dstComponentFeatureId = symbolTable.featureMap.get(dstFeatureId).get.identifier

    return ArchitectureTemplate.connection(
      s"${srcComponentId}.${srcComponentFeatureId}",
      s"${dstComponentId}.${dstComponentFeatureId}")
  }

  def allowConnection(c: ConnectionInstance, srcComponent: ir.Component): B = {
    val str = s"${CommonUtil.getName(c.name)}  from  ${CommonUtil.getName(srcComponent.identifier)}"

    if (c.src.component == c.dst.component) {
      reporter.info(None(), Util.toolName, s"Skipping: Port connected to itself. $str")
      return F
    }
    if (c.kind != ir.ConnectionKind.Port) {
      reporter.info(None(), Util.toolName, s"Skipping: ${c.kind} connection.  $str")
      return F
    }

    val allowedComponents: ISZ[ir.ComponentCategory.Type] = {
      if (arsitOptions.devicesAsThreads) ISZ(ir.ComponentCategory.Device, ir.ComponentCategory.Thread)
      else ISZ(ir.ComponentCategory.Thread)
    }

    val catSrc = symbolTable.airComponentMap.get(c.src.component.name).get.category
    val catDest = symbolTable.airComponentMap.get(c.dst.component.name).get.category

    if (!ISZOps(allowedComponents).contains(catSrc) || !ISZOps(allowedComponents).contains(catDest)) {
      reporter.info(None(), Util.toolName, s"Skipping: connection between ${catSrc} to ${catDest}.  $str")
      return F
    }

    if (seenConnections.contains(c.src.feature.get) && ISZOps(seenConnections.get(c.src.feature.get).get).contains(c.dst.feature.get)) {
      reporter.info(None(), Util.toolName, s"Skipping: already handled connection: ${c.src.feature.get} to ${c.dst.feature.get}")
      return F
    }

    val seq: ISZ[ir.Name] =
      if (!seenConnections.contains(c.src.feature.get)) ISZ(c.dst.feature.get)
      else seenConnections.get(c.src.feature.get).get :+ c.dst.feature.get

    seenConnections = seenConnections + (c.src.feature.get ~> seq)

    return T
  }

  def addExeResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createExeResource(Util.pathAppend(baseDir, paths), content, overwrite)
  }

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(baseDir, paths), content, overwrite)
  }
}
