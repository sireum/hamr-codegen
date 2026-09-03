// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.c.connections

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, ISZValue, Store}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.plugins.StoreUtil
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.MicrokitPlugin
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MemoryRegion
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

object CConnectionProviderPlugin {

  // Queue headers/implementations are per-type and shared across components, so peek is gated
  // model-wide: emit it only when some component actually has an R2U2 monitor to consume it.
  @pure def modelHasR2U2Monitor(symbolTable: SymbolTable): B = {
    return ops.ISZOps(symbolTable.getThreads()).exists((t: AadlThread) =>
      ConnectionUtil.needsPeekApi(t, symbolTable))
  }

  val KEY_CConnectionProviderPlugin: String = "KEY_CConnectionProviderPlugin"

  @strictpure def getCConnectionStore(store: Store): ISZ[ConnectionStore] =
    getCConnectionStoreOpt(store).get.elements

  @strictpure def getCConnectionStoreOpt(store: Store): Option[ISZValue[ConnectionStore]] =
    store.get(KEY_CConnectionProviderPlugin).asInstanceOf[Option[ISZValue[ConnectionStore]]]

  @pure def getTypeObjectNames(store: Store): ISZ[String] = {
    return (for(s <- getCConnectionStore(store); tc <- s.typeApiContributions) yield tc.objectName)
  }

  @pure def getTypeSimpleObjectNames(store: Store): ISZ[String] = {
    return (for(s <- getCConnectionStore(store); tc <- s.typeApiContributions) yield tc.objectSimpleName)
  }

  @pure def getMakeFileEntries(store: Store): ISZ[ST] = {
    return (for(s <- getCConnectionStore(store); tc <- s.typeApiContributions) yield tc.buildEntry)
  }

  @strictpure def putCConnectionStore(values: ISZ[ConnectionStore], store: Store): Store =
    store + KEY_CConnectionProviderPlugin ~> ISZValue(values)
}

@sig trait CConnectionProviderPlugin extends MicrokitPlugin {
  @strictpure def hasHandled(store: Store): B = store.contains(CConnectionProviderPlugin.KEY_CConnectionProviderPlugin)

  @strictpure def canHandle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): B =
    options.platform == HamrCli.CodegenHamrPlatform.Microkit &&
      !isDisabled(store) &&
      CTypePlugin.getCTypeProvider(store).nonEmpty &&
      !hasHandled(store)

  override def handle(model: Aadl, options: HamrCli.CodegenOption, types: AadlTypes, symbolTable: SymbolTable, store: Store, reporter: Reporter): (Store, ISZ[Resource]) = {
    val peekApiModelWide: B = CConnectionProviderPlugin.modelHasR2U2Monitor(symbolTable)
    var localStore = store
    var resources = ISZ[Resource]()

    var ret: ISZ[ConnectionStore] = ISZ()

    val cTypeProvider = CTypePlugin.getCTypeProvider(localStore).get

    for (srcThread <- symbolTable.getThreads()) {

      for (srcPort <- srcThread.getPorts()
           if srcPort.direction == Direction.Out && symbolTable.outConnections.contains(srcPort.path)) {

        var codeContributions: Map[ISZ[String], UberConnectionContributions] = Map.empty

        for (outConnection <- symbolTable.getOutConnections(srcPort.path)) {
          symbolTable.componentMap.get(outConnection.dst.component.name).get match {
            case dstThread: AadlThread =>

              val dstPort = symbolTable.featureMap.get(outConnection.dst.feature.get.name).get

              codeContributions = codeContributions + dstThread.path ~>
                ConnectionUtil.processInPort(dstThread, dstPort.asInstanceOf[AadlPort],
                  Some(srcPort),cTypeProvider, symbolTable)

            case x =>
              halt(s"Only handling thread to thread connections currently: $x")
          }
        } // end processing out connections for the source port

        val senderContributions = ConnectionUtil.processOutPort(srcThread, srcPort, codeContributions, cTypeProvider, symbolTable)
        codeContributions = codeContributions + srcThread.path ~> senderContributions

        val typeApiContributions: ISZ[TypeApiContributions] =
          (Set.empty[TypeApiContributions] ++ (for (rc <- codeContributions.values) yield
            MicrokitTypeUtil.getTypeApiContributions(rc.aadlType, cTypeProvider, rc.queueSize, peekApiModelWide))).elements

        ret = ret :+
          DefaultConnectionStore(
            systemContributions =
              DefaultSystemContributions(
                sharedMemoryRegionContributions = senderContributions.sharedMemoryMapping,
                channelContributions = ISZ()),
            typeApiContributions = typeApiContributions,
            senderName = srcThread.path,
            codeContributions = codeContributions)
      } // end processing connections for source port

      // now handle unconnected ports of the source thread.
      // If the thread is plugin-generated (e.g. the runtime monitor), suppress shared memory
      // regions — those ports are wired at the meta.py template level, not via HAMR queues.
      val isPluginThread: B = StoreUtil.isSynthetic(srcThread.path, localStore)
      for (unconnectedPort <- srcThread.getPorts().filter((p: AadlPort) =>
        !symbolTable.inConnections.contains(p.path) &&
        !symbolTable.outConnections.contains(p.path))) {
        val srcThreadContributions: UberConnectionContributions =
          if (unconnectedPort.direction == Direction.In) {
            ConnectionUtil.processInPort(
              dstThread = srcThread, dstPort = unconnectedPort,
              srcPort = None(),
              cTypeProvider = cTypeProvider,
              symbolTable = symbolTable)
          } else {
            ConnectionUtil.processOutPort(
              srcThread = srcThread, srcPort = unconnectedPort,
              receiverContributions = Map.empty,
              cTypeProvider = cTypeProvider,
              symbolTable = symbolTable)
          }

        val typeApiContributions =
          MicrokitTypeUtil.getTypeApiContributions(srcThreadContributions.aadlType, cTypeProvider, srcThreadContributions.queueSize, peekApiModelWide)

        val sharedMemoryRegionContributions: ISZ[MemoryRegion] =
          if (isPluginThread) ISZ() else srcThreadContributions.sharedMemoryMapping

        ret = ret :+
          DefaultConnectionStore(
            systemContributions =
              DefaultSystemContributions(
                sharedMemoryRegionContributions = sharedMemoryRegionContributions,
                channelContributions = ISZ()),
            typeApiContributions = ISZ(typeApiContributions),
            senderName = srcThread.path,
            codeContributions = Map.empty[ISZ[String], UberConnectionContributions] + srcThread.path ~> srcThreadContributions)
      }
    } // end processing connections for threads

    // Force-touched types (e.g. MCS scheduler types like hamr::SchedState and
    // hamr::Schedule) are registered via CTypePlugin.addToForceTouchedTypes during
    // model transformation.  The type struct definitions are already generated by
    // CTypePlugin (which uses getAllTouchedTypes), but the queue wrapper C files
    // (sb_queue_*.h/.c) are only generated here in CConnectionProviderPlugin from
    // TypeApiContributions.  Normally those contributions come from port connections,
    // but force-touched types may not be referenced by any thread port (e.g. when
    // runtime monitoring is disabled the scheduler still needs the queue wrappers
    // for its shared-memory broadcast regions).  We add them here so the queue files,
    // Makefile object entries, and sb_types.h includes are generated.
    val existingTypeNames: Set[String] = Set.empty[String] ++
      (for (entry <- ret; tc <- entry.typeApiContributions) yield tc.aadlType.name)
    for (forcedTypeClassifier <- CTypePlugin.getForceTouchedTypes(localStore)) {
      if (!existingTypeNames.contains(forcedTypeClassifier)) {
        types.typeMap.get(forcedTypeClassifier) match {
          case Some(aadlType) =>
            val typeApiContrib = MicrokitTypeUtil.getTypeApiContributions(aadlType, cTypeProvider, 1, peekApiModelWide)
            ret = ret :+
              DefaultConnectionStore(
                systemContributions =
                  DefaultSystemContributions(
                    sharedMemoryRegionContributions = ISZ(),
                    channelContributions = ISZ()),
                typeApiContributions = ISZ(typeApiContrib),
                senderName = ISZ(),
                codeContributions = Map.empty)
          case _ =>
        }
      }
    }

    val baseTypesIncludePath = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/${MicrokitCodegen.dirInclude}"

    var typeHeaderFilenames: ISZ[String] = ISZ(MicrokitTypeUtil.cAadlTypesFilename)

    // A queue wrapper is per (type, queue size), which is exactly what its filename
    // encodes -- but ret holds one entry per connection, and every connection
    // carrying a given type contributes the same wrapper again.  Emitting straight
    // from that loop wrote the same .h/.c once per connection: identical content,
    // but written repeatedly and listed repeatedly in the codegen report (isolette
    // emitted one of them five times).  Collecting by filename first makes one
    // wrapper per type structural, rather than something a later pass has to undo
    // -- which is what uniqueTypeHeaderFilenames below already had to do for the
    // include list.
    var typeApis: HashSMap[String, TypeApiContributions] = HashSMap.empty
    for (entry <- ret; tc <- entry.typeApiContributions) {
      typeHeaderFilenames = typeHeaderFilenames :+ tc.headerFilename
      typeApis = typeApis + tc.headerFilename ~> tc
    }

    val srcPath = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/${MicrokitCodegen.dirSrc}"
    for (tc <- typeApis.values) {
      val headerPath = s"$baseTypesIncludePath/${tc.headerFilename}"
      resources = resources :+ ResourceUtil.createResourceH(
        path = headerPath, content = tc.header, overwrite = T, isDatatype = T)

      val implPath = s"$srcPath/${tc.implementationFilename}"
      resources = resources :+ ResourceUtil.createResourceH(
        path = implPath, content = tc.implementation, overwrite = T, isDatatype = T)
    }

    val uniqueTypeHeaderFilenames: ISZ[String] = (Set.empty[String] ++ typeHeaderFilenames).elements
    val allTypesContent =
      st"""#pragma once
          |
          |${CommentTemplate.doNotEditComment_slash}
          |
          |${(for (i <- uniqueTypeHeaderFilenames) yield st"#include <$i>", "\n")}
          |"""
    val allTypesPath = s"$baseTypesIncludePath/${MicrokitTypeUtil.cAllTypesFilename}"
    resources = resources :+ ResourceUtil.createResourceH(
      path = allTypesPath, content = allTypesContent, overwrite = T, isDatatype = T)

    return (CConnectionProviderPlugin.putCConnectionStore(ret, localStore), resources)
  }
}

@datatype class DefaultCConnectionProviderPlugin extends CConnectionProviderPlugin  {

  @strictpure override def name: String = "DefaultCConnectionProviderPlugin"
}
