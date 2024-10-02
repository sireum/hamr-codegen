// #Sireum

package org.sireum.hamr.arsit

import org.sireum._
import org.sireum.hamr.arsit.Util.nameProvider
import org.sireum.hamr.arsit.gcl.GumboGen
import org.sireum.hamr.arsit.plugin.BehaviorEntryPointProviderPlugin.BehaviorEntryPointObjectContributions
import org.sireum.hamr.arsit.plugin._
import org.sireum.hamr.arsit.templates.{ApiTemplate, StubTemplate, TestTemplate}
import org.sireum.hamr.arsit.util.ArsitOptions
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.{FileResource, Marker}
import org.sireum.hamr.codegen.common.plugin.Plugin
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes}
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir._

@record class StubGenerator(dirs: ProjectDirectories,
                            rootSystem: AadlSystem,
                            arsitOptions: ArsitOptions,
                            symbolTable: SymbolTable,
                            aadlTypes: AadlTypes,
                            previousPhase: Result) {

  val basePackage: String = arsitOptions.packageName
  var seenComponents: HashSet[String] = HashSet.empty
  var resources: ISZ[FileResource] = ISZ()

  def generate(plugins: MSZ[Plugin]): Result = {

    gen(rootSystem, plugins)

    return ArsitResult(
      resources = previousPhase.resources() ++ resources,
      auxResources = previousPhase.auxResources,
      maxPort = previousPhase.maxPort,
      maxComponent = previousPhase.maxComponent,
      maxConnection = previousPhase.maxConnection
    )
  }

  def gen(m: AadlComponent, plugins: MSZ[Plugin]): Unit = {
    m match {
      case s: AadlSystem => genContainer(s, plugins)
      case s: AadlProcess => genContainer(s, plugins)

      case s: AadlThreadGroup => genThreadGroup(s, plugins)

      case _ =>
        for (_c <- m.subComponents) {
          gen(_c, plugins)
        }
    }
  }

  def genContainer(m: AadlComponent, plugins: MSZ[Plugin]): Unit = {
    assert(m.isInstanceOf[AadlSystem] || m.isInstanceOf[AadlProcess])

    for (c <- m.subComponents) {
      c match {
        case s: AadlSystem => genContainer(s, plugins)
        case s: AadlProcess => genContainer(s, plugins)

        case s: AadlThreadGroup => genThreadGroup(s, plugins)

        case s: AadlThread => genThread(s, plugins)

        case s: AadlDevice =>
          if (arsitOptions.devicesAsThreads) {
            genThread(s, plugins)
          }

        case s: AadlSubprogram => // ignore

        case _ =>
          reporter.info(None(), Util.toolName, s"Skipping: ${c.component.category} component: ${c.path}")
      }
    }
  }

  def genThreadGroup(m: AadlThreadGroup, plugins: MSZ[Plugin]): Unit = {

    for (c <- m.subComponents) {
      c match {
        case s: AadlThread => genThread(s, plugins)

        case x => halt(s"Unexpected Thread Group subcomponent: ${x}")
      }
    }
  }

  def genThread(m: AadlThreadOrDevice, plugins: MSZ[Plugin]): Unit = {
    assert(!m.isInstanceOf[AadlDevice] || arsitOptions.devicesAsThreads)

    val names = nameProvider(m.component, basePackage)
    val filename: String = Util.pathAppend(dirs.componentDir, ISZ(names.packagePath, s"${names.componentSingletonType}.scala"))

    val ports: ISZ[Port] = Util.getPorts(m, aadlTypes, basePackage, z"-1000")

    resources = resources ++ TestTemplate.bridgeTestApis(basePackage, names, dirs, ports)

    val bridgeTestSuite: ST = TestTemplate.bridgeTestSuite(names.packageName, names)
    addResource(dirs.testBridgeDir, ISZ(names.packagePath, s"${names.testName}.scala"), bridgeTestSuite, F)

    if (seenComponents.contains(filename)) {
      return
    }

    val resolvedAnnexClauseInfos: ISZ[AnnexClauseInfo] = symbolTable.annexClauseInfos.get(m.path) match {
      case Some(infos) => infos
      case _ => ISZ()
    }

    val bcpIndex = ArsitPlugin.getBridgeCodeProvidersIndices(plugins)(0)
    val bridgeContributions = plugins(bcpIndex).asInstanceOf[BridgeCodeProviderPlugin].generate(
      nameProvider = names,
      component = m,
      ports = ports,

      symbolTable = symbolTable,
      aadlTypes = aadlTypes,
      reporter = reporter)

    // now call the entrypoint provider
    // TODO: assume first provider wins??
    val epIndex = ArsitPlugin.getEntryPointProviderIndices(plugins, m, resolvedAnnexClauseInfos, arsitOptions, symbolTable, aadlTypes)(0)

    val entryPointContributions = plugins(epIndex).asInstanceOf[EntryPointProviderPlugin].handleEntryPointProvider(
      m, names, ports,

      resolvedAnnexClauseInfos,

      bridgeContributions.entryPointTemplate,

      arsitOptions, symbolTable, aadlTypes, dirs, reporter)

    val bridgeCodeST = bridgeContributions.e(entryPointContributions)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.bridge}.scala"), bridgeCodeST, T)
    resources = resources ++ bridgeContributions.resources ++ entryPointContributions.resources


    val integrationContracts = GumboGen.processIntegrationContract(m, symbolTable, aadlTypes, basePackage)

    val api = ApiTemplate.api(
      names.packageName,
      basePackage,
      names,
      ports,
      integrationContracts)

    addResource(dirs.bridgeDir, ISZ(names.packagePath, s"${names.api}.scala"), api, T)

    if (ArsitPlugin.canHandleBehaviorProviders(plugins, m, resolvedAnnexClauseInfos)) {
      // a plugin has indicated it will fully provide the behavior code

      var firstCome = F  // only one plugin allowed
      for (p <- plugins if !firstCome && ArsitPlugin.canHandleBP(p, m, resolvedAnnexClauseInfos)) {
        firstCome = T

        resources = resources ++ p.asInstanceOf[BehaviorProviderPlugin].handleBehaviorProvider(
          component = m,
          resolvedAnnexSubclauses = resolvedAnnexClauseInfos,
          suggestedFilename = filename,
          componentDirectory = ISZ(dirs.componentDir, names.packagePath),
          symbolTable = symbolTable,
          aadlTypes = aadlTypes,
          reporter = reporter)
      }
    } else {
      var blocks: ISZ[ST] = ISZ()

      var behaviorCodeContributions: ISZ[BehaviorEntryPointObjectContributions] = ISZ()
      for (entryPoint <- ISZ(EntryPoints.initialise, EntryPoints.compute, EntryPoints.finalise)) {
        entryPoint match {
          case EntryPoints.compute if m.isSporadic() =>
            val inEventPorts = m.features.filter(
              f => f.isInstanceOf[AadlFeatureEvent] &&
                f.asInstanceOf[AadlFeatureEvent].direction == Direction.In).map((m: AadlFeature) => m.asInstanceOf[AadlPort])
            var isFirst = T
            for (inEventPort <- inEventPorts) {
              val methodSig: String = BehaviorEntryPointElementProvider.genMethodSignature(entryPoint, names, Some(inEventPort))
              val defaultMethodBody: ST = BehaviorEntryPointElementProvider.genComputeMethodBody(Some(inEventPort), m, isFirst, arsitOptions.excludeImpl)

              behaviorCodeContributions = behaviorCodeContributions :+ BehaviorEntryPointProviders.offer(
                entryPoint, Some(inEventPort), m, names, arsitOptions.excludeImpl, methodSig, defaultMethodBody, resolvedAnnexClauseInfos,
                basePackage, symbolTable, aadlTypes, dirs, arsitOptions, plugins, reporter)

              isFirst = F
            }
          case _ =>
            val methodSig: String = BehaviorEntryPointElementProvider.genMethodSignature(entryPoint, names, None())
            val defaultMethodBody: ST = entryPoint match {
              case EntryPoints.compute => BehaviorEntryPointElementProvider.genComputeMethodBody(None(), m, T, arsitOptions.excludeImpl)
              case _ => BehaviorEntryPointElementProvider.genMethodBody(entryPoint, m, arsitOptions.excludeImpl)
            }

            behaviorCodeContributions = behaviorCodeContributions :+ BehaviorEntryPointProviders.offer(entryPoint, None(),
              m, names,
              arsitOptions.excludeImpl, methodSig, defaultMethodBody, resolvedAnnexClauseInfos,
              basePackage, symbolTable, aadlTypes, dirs, arsitOptions, plugins, reporter)
        }
      }

      genSubprograms(m) match {
        case Some(x) =>
          blocks = blocks :+ x
          halt("Need to revisit subprograms")
        case _ =>
      }

      behaviorCodeContributions = behaviorCodeContributions :+
        BehaviorEntryPointProviders.finalise(resolvedAnnexClauseInfos, m, names, basePackage, symbolTable, aadlTypes, dirs, arsitOptions, plugins, reporter)

      val markers = BehaviorEntryPointProviders.getMarkers(behaviorCodeContributions)
      val componentImpl: ST = BehaviorEntryPointElementProvider.genComponentImpl(names, behaviorCodeContributions)
      addResourceWithMarkers(filename, ISZ(), componentImpl, markers, F)

      // add external resources
      resources = resources ++ behaviorCodeContributions.flatMap((f: BehaviorEntryPointObjectContributions) => f.resources)
    }

    seenComponents = seenComponents + filename
  }

  def genSubprograms(s: AadlComponent): Option[ST] = {
    val m = s.component
    val subprograms: ISZ[(ST, ST)] = m.subComponents.filter(p => p.category == ComponentCategory.Subprogram).map(p => {
      // only expecting in or out parameters
      assert(ops.ISZOps(Util.getFeatureEnds_DEPRECATED(p.features))
        .forall(f => f.category == FeatureCategory.Parameter && f.direction != Direction.InOut))

      val methodName = CommonUtil.getLastName(p.identifier)
      val params: ISZ[String] = Util.getFeatureEnds_DEPRECATED(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isInFeature(f))
        .map(param => {
          val pType = Util.getFeatureEndType(param, aadlTypes)
          s"${CommonUtil.getLastName(param.identifier)} : ${pType.nameProvider.qualifiedReferencedTypeName}"
        })
      val rets: ISZ[FeatureEnd] = Util.getFeatureEnds_DEPRECATED(p.features).filter(f => f.category == FeatureCategory.Parameter && CommonUtil.isOutFeature(f))
      assert(rets.size <= 1, s"Expecting a single out param but found ${rets.size}")


      val (returnType, exampleType): (Option[String], Option[ST]) =
        if (rets.isEmpty) {
          (None(), None())
        }
        else {
          val rType: AadlType = Util.getFeatureEndType(rets(0), aadlTypes)
          val _exampleValue: String = rType.nameProvider.example()
          val returnType = rType.nameProvider.qualifiedReferencedTypeName

          (Some(returnType), Some(st"${_exampleValue}"))
        }

      StubTemplate.subprogram(methodName, params, returnType, exampleType)
    })

    if (subprograms.nonEmpty) {
      val names = nameProvider(m, basePackage)
      val objectName = s"${names.componentSingletonType}_subprograms"

      val body = StubTemplate.slangBody(
        slangAnnotation = "@ext ",
        objectName = objectName,
        body = subprograms.map(m => m._1))

      if (!CommonUtil.isThread(m)) {
        val a = StubTemplate.slangPreamble(T, basePackage, names.packageName, ISZ(), ISZ(body))
        addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}.scala"), a, T)
      }

      val b = StubTemplate.slangPreamble(
        F,
        basePackage,
        names.packageName,
        ISZ(),
        ISZ(StubTemplate.slangBody(
          slangAnnotation = "",
          objectName = s"${objectName}_Ext",
          body = subprograms.map(m => m._2))))
      addResource(dirs.componentDir, ISZ(names.packagePath, s"${objectName}_Ext.scala"), b, F)

      return Some(body)
    } else {
      return None[ST]()
    }
  }

  def addResource(baseDir: String, paths: ISZ[String], content: ST, overwrite: B): Unit = {
    addResourceWithMarkers(baseDir, paths, content, ISZ(), overwrite)
  }

  def addResourceWithMarkers(baseDir: String, paths: ISZ[String], content: ST, markers: ISZ[Marker], overwrite: B): Unit = {
    resources = resources :+ ResourceUtil.createResourceWithMarkers(Util.pathAppend(baseDir, paths), content, markers, overwrite)
  }
}
