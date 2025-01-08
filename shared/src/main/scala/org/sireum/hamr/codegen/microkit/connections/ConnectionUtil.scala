// #Sireum
package org.sireum.hamr.codegen.microkit.connections

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureEvent, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.types.{QueueTemplate, TypeStore, TypeUtil}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.ir.{ConnectionInstance, Name}

object ConnectionUtil {

  def getPortType(p: AadlPort): AadlType = {
    val ret: AadlType = p match {
      case _: AadlEventPort => TypeUtil.eventPortType
      case p: AadlEventDataPort => p.aadlType
      case p: AadlDataPort => p.aadlType
      case _ => halt("Infeasible")
    }
    return ret
  }

  def processInPort(dstThread: AadlThread,
                    dstPort: AadlPort,
                    srcPort: Option[AadlPort],
                    outConnection: Option[ConnectionInstance],
                    typeStore: Map[AadlType, TypeStore],
                    symbolTable: SymbolTable,
                   ): DefaultConnectionContributions = {

    val aadlType: AadlType = (dstPort, srcPort) match {
      case (p, None()) => getPortType(p)
      case (srcPort, Some(dstPort)) =>
        assert(
          (srcPort.isInstanceOf[AadlDataPort] && dstPort.isInstanceOf[AadlDataPort]) ||
            (srcPort.isInstanceOf[AadlEventPort] && dstPort.isInstanceOf[AadlEventPort]) ||
            (srcPort.isInstanceOf[AadlEventDataPort] && dstPort.isInstanceOf[AadlEventDataPort]),
          s"Connected ports must be of the same type: ${srcPort.path} -> ${dstPort.path}")

        assert(getPortType(srcPort) == getPortType(dstPort),
          s"Currently expecting connected ports to have the same type ${outConnection.get.name}")

        getPortType(srcPort)
    }

    val isDataPort = dstPort.isInstanceOf[AadlDataPort]
    val isEventPort = dstPort.isInstanceOf[AadlEventPort]
    val isEventDataPort = dstPort.isInstanceOf[AadlEventDataPort]

    val dstQueueSize: Z = if (isDataPort) 1
    else dstPort.asInstanceOf[AadlFeatureEvent].queueSize

    val typeName = typeStore.get(aadlType).get

    val sharedMemTypeName = QueueTemplate.getTypeQueueTypeName(typeName.typeName, dstQueueSize)

    val sharedVarName = QueueTemplate.getClientDequeueSharedVarName(dstPort.identifier, dstQueueSize)
    val recvQueueTypeName = QueueTemplate.getClientRecvQueueTypeName(typeName.typeName, dstQueueSize)
    val recvQueueVarName = QueueTemplate.getClientRecvQueueName(dstPort.identifier)

    var methodApiSigs: ISZ[ST] = ISZ()
    var methodApis: ISZ[ST] = ISZ()
    if (isEventPort || isEventDataPort) {
      methodApiSigs = methodApiSigs :+
        QueueTemplate.getClientIsEmptyMethodSig(dstPort.identifier) :+
        QueueTemplate.getClientGetterMethodPollSig(dstPort.identifier, typeName.typeName, isEventPort) :+
        QueueTemplate.getClientGetterMethodSig(dstPort.identifier, typeName.typeName, isEventPort)
      methodApis = methodApis :+
        QueueTemplate.getClientIsEmptyMethod(dstPort.identifier, typeName.typeName, dstQueueSize) :+
        QueueTemplate.getClientGetterMethodPoll(dstPort.identifier, typeName.typeName, dstQueueSize, isEventPort) :+
        QueueTemplate.getClientGetterMethod(dstPort.identifier, typeName.typeName, isEventPort)
    } else {
      methodApiSigs = methodApiSigs :+
        QueueTemplate.getClientGetterMethodSig(dstPort.identifier, typeName.typeName, F)
      methodApis = methodApis :+
        QueueTemplate.getClientDataGetterMethod(dstPort.identifier, typeName.typeName, dstQueueSize, aadlType)
    }

    var userMethodSignatures: ISZ[ST] = ISZ()
    var userMethodDefaultImpls: ISZ[ST] = ISZ()
    var computeContributions: ISZ[ST] = ISZ()
    if (!dstThread.toVirtualMachine(symbolTable)) {
      if (dstThread.isSporadic()) {
        userMethodSignatures = userMethodSignatures :+ QueueTemplate.getClientEventHandlerMethodSig(dstPort.identifier)

        if (isEventPort || isEventDataPort) {
          computeContributions = computeContributions :+ QueueTemplate.getClientSporadicComputeContributions(dstPort.identifier)
        }

        userMethodDefaultImpls = userMethodDefaultImpls :+ QueueTemplate.getClientSporadicDefaultImplContributions(dstPort.identifier)
      }
    }

    val memoryRegionName: ISZ[String] = if(srcPort.nonEmpty) srcPort.get.path else dstPort.path

    return DefaultConnectionContributions(
      portName = dstPort.path,
      portPriority = None(),
      headerImportContributions = ISZ(),
      implementationImportContributions = ISZ(),
      userMethodSignatures = userMethodSignatures,
      userMethodDefaultImpls = userMethodDefaultImpls,
      defineContributions = ISZ(),
      globalVarContributions = ISZ(
        PortVaddr(s"volatile $sharedMemTypeName", s"*$sharedVarName"),
        QueueVaddr(recvQueueTypeName, recvQueueVarName)
      ),
      apiMethodSigs = methodApiSigs,
      apiMethods = methodApis,
      initContributions = ISZ(QueueTemplate.getClientRecvInitMethodCall(dstPort.identifier, typeName.typeName, dstQueueSize)),
      computeContributions = computeContributions,
      sharedMemoryMapping = ISZ(
        PortSharedMemoryRegion(
          outgoingPortPath = memoryRegionName,
          queueSize = dstQueueSize,
          varAddr = sharedVarName,
          perms = ISZ(Perm.READ),
          sizeInKiBytes = Util.defaultPageSizeInKiBytes,
          physicalAddressInKiBytes = None())
      ),
      aadlType = aadlType,
      queueSize = dstQueueSize
    )
  }

  def processOutPort(srcPort: AadlPort,
                     receiverContributions: Map[ISZ[String], ConnectionContributions],
                     typeStore: Map[AadlType, TypeStore]): DefaultConnectionContributions = {
    val isEventPort = srcPort.isInstanceOf[AadlEventPort]

    var sharedMemoryMappings: ISZ[MemoryRegion] = ISZ()
    var sharedMemoryVars: ISZ[GlobalVarContribution] = ISZ()
    var initContributions: ISZ[ST] = ISZ()
    var srcPutContributions: ISZ[ST] = ISZ()

    var uniqueQueueSizes: Set[Z] = Set.empty

    val senderPortType: AadlType = getPortType(srcPort)

    for (receiverContribution <- receiverContributions.values if !uniqueQueueSizes.contains(receiverContribution.queueSize)) {
      uniqueQueueSizes = uniqueQueueSizes + receiverContribution.queueSize

      if (receiverContribution.aadlType != senderPortType) {
        // TODO change to reporter or ensure linter prevents thsi
        halt("Connected ports must have the same type")
      }

      val typeName = typeStore.get(receiverContribution.aadlType).get

      val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, receiverContribution.queueSize)
      srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
        sharedMemoryVarName = sharedMemVarName,
        queueElementTypeName = typeName.typeName,
        queueSize = receiverContribution.queueSize,
        isEventPort = isEventPort)

      val sPortType: String = typeStore.get(receiverContribution.aadlType).get.typeName
      val queueType = QueueTemplate.getTypeQueueTypeName(sPortType, receiverContribution.queueSize)
      val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, receiverContribution.queueSize)
      sharedMemoryVars = sharedMemoryVars :+ QueueVaddr(s"volatile $queueType", s"*$varName")

      initContributions = initContributions :+ QueueTemplate.getQueueInitMethod(varName, sPortType, receiverContribution.queueSize)

      sharedMemoryMappings = sharedMemoryMappings :+
        PortSharedMemoryRegion(
          outgoingPortPath = srcPort.path,
          queueSize = receiverContribution.queueSize,
          varAddr = varName,
          perms = ISZ(Perm.READ, Perm.WRITE),
          sizeInKiBytes = Util.defaultPageSizeInKiBytes,
          physicalAddressInKiBytes = None()
        )
    }

    if (receiverContributions.isEmpty) {
      val typeName = typeStore.get(senderPortType).get

      val queueSize = 1

      val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
      srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
        sharedMemoryVarName = sharedMemVarName,
        queueElementTypeName = typeName.typeName,
        queueSize = queueSize,
        isEventPort = isEventPort)

      val sPortType: String = typeStore.get(senderPortType).get.typeName
      val queueType = QueueTemplate.getTypeQueueTypeName(sPortType, queueSize)
      val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
      sharedMemoryVars = sharedMemoryVars :+ QueueVaddr(s"volatile $queueType", s"*$varName")

      initContributions = initContributions :+ QueueTemplate.getQueueInitMethod(varName, sPortType, queueSize)

      sharedMemoryMappings = sharedMemoryMappings :+
        PortSharedMemoryRegion(
          outgoingPortPath = srcPort.path,
          queueSize = queueSize,
          varAddr = varName,
          perms = ISZ(Perm.READ, Perm.WRITE),
          sizeInKiBytes = Util.defaultPageSizeInKiBytes,
          physicalAddressInKiBytes = None()
        )
    }

    val sPortType: String = typeStore.get(senderPortType).get.typeName
    val apiMethodSig = QueueTemplate.getClientPutMethodSig(srcPort.identifier, sPortType, isEventPort)
    val apiMethod = QueueTemplate.getClientPutMethod(srcPort.identifier, sPortType, srcPutContributions, isEventPort)

    return DefaultConnectionContributions(
      portName = srcPort.path,
      portPriority = None(),
      headerImportContributions = ISZ(),
      implementationImportContributions = ISZ(),
      userMethodSignatures = ISZ(),
      userMethodDefaultImpls = ISZ(),
      defineContributions = ISZ(),
      globalVarContributions = sharedMemoryVars,
      apiMethodSigs = ISZ(apiMethodSig),
      apiMethods = ISZ(apiMethod),
      initContributions = initContributions,
      computeContributions = ISZ(),
      sharedMemoryMapping = sharedMemoryMappings,

      aadlType = senderPortType,
      queueSize = 1 // outgoing port so queue size not relevant
    )
  }

}
