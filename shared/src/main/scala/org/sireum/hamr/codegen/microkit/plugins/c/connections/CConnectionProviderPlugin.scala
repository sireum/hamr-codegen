// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.c.connections

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{BoolValue, ISZValue, Store}
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.symbols.{AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.codegen.common.util.{HamrCli, ResourceUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.{MicrokitFinalizePlugin, MicrokitPlugin}
import org.sireum.hamr.codegen.microkit.plugins.c.types.CTypePlugin
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.ir.{Aadl, Direction}
import org.sireum.message.Reporter

object CConnectionProviderPlugin {
  val KEY_CConnectionProviderPlugin: String = "KEY_CConnectionProviderPlugin"

  @strictpure def getCConnectionStore(store: Store): ISZ[ConnectionStore] =
    getCConnectionStoreOpt(store).get.elements

  @strictpure def getCConnectionStoreOpt(store: Store): Option[ISZValue[ConnectionStore]] =
    store.get(KEY_CConnectionProviderPlugin).asInstanceOf[Option[ISZValue[ConnectionStore]]]

  @pure def getTypeObjectNames(store: Store): ISZ[String] = {
    return (for(s <- getCConnectionStore(store); tc <- s.typeApiContributions) yield tc.objectName)
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

        val senderContributions = ConnectionUtil.processOutPort(srcPort, codeContributions, cTypeProvider)
        codeContributions = codeContributions + srcThread.path ~> senderContributions

        val typeApiContributions: ISZ[TypeApiContributions] =
          (Set.empty[TypeApiContributions] ++ (for (rc <- codeContributions.values) yield
            MicrokitTypeUtil.getTypeApiContributions(rc.aadlType, cTypeProvider, rc.queueSize))).elements

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

      // now handle unconnected ports of the source thread
      for (unconnectedPort <- srcThread.getPorts().filter(p => !symbolTable.inConnections.contains(p.path) && !symbolTable.outConnections.contains(p.path))) {
        val srcThreadContributions: UberConnectionContributions =
          if (unconnectedPort.direction == Direction.In) {
            ConnectionUtil.processInPort(
              dstThread = srcThread, dstPort = unconnectedPort,
              srcPort = None(),
              cTypeProvider = cTypeProvider,
              symbolTable = symbolTable)
          } else {
            ConnectionUtil.processOutPort(unconnectedPort, Map.empty, cTypeProvider)
          }

        val typeApiContributions =
          MicrokitTypeUtil.getTypeApiContributions(srcThreadContributions.aadlType, cTypeProvider, srcThreadContributions.queueSize)

        ret = ret :+
          DefaultConnectionStore(
            systemContributions =
              DefaultSystemContributions(
                sharedMemoryRegionContributions = srcThreadContributions.sharedMemoryMapping,
                channelContributions = ISZ()),
            typeApiContributions = ISZ(typeApiContributions),
            senderName = srcThread.path,
            codeContributions = Map.empty[ISZ[String], UberConnectionContributions] + srcThread.path ~> srcThreadContributions)
      }
    } // end processing connections for threads


    val baseTypesIncludePath = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/${MicrokitCodegen.dirInclude}"

    var typeHeaderFilenames: ISZ[String] = ISZ(MicrokitTypeUtil.cAadlTypesFilename)

    for (entry <- ret) {
      val srcPath = s"${options.sel4OutputDir.get}/${MicrokitTypeUtil.cTypesDir}/${MicrokitCodegen.dirSrc}"

      for (tc <- entry.typeApiContributions) {
        typeHeaderFilenames = typeHeaderFilenames :+ tc.headerFilename

        val headerPath = s"$baseTypesIncludePath/${tc.headerFilename}"
        resources = resources :+ ResourceUtil.createResourceH(
          path = headerPath, content = tc.header, overwrite = T, isDatatype = T)

        val implPath = s"$srcPath/${tc.implementationFilename}"
        resources = resources :+ ResourceUtil.createResourceH(
          path = implPath, content = tc.implementation, overwrite = T, isDatatype = T)
      }
    }

    val allTypesContent =
      st"""#pragma once
          |
          |${MicrokitUtil.doNotEdit}
          |
          |${(for (i <- typeHeaderFilenames) yield st"#include <$i>", "\n")}
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
