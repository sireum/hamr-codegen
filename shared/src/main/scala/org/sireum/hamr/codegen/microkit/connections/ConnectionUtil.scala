// #Sireum
package org.sireum.hamr.codegen.microkit.connections

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureEvent, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.types.{QueueTemplate, TypeStore, MicrokitTypeUtil}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.ir.{ConnectionInstance, Name}

object ConnectionUtil {

  def getPortType(p: AadlPort): AadlType = {
    val ret: AadlType = p match {
      case _: AadlEventPort => MicrokitTypeUtil.eventPortType
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
                    typeStore: TypeStore,
                    symbolTable: SymbolTable): UberConnectionContributions = {

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

        typeStore.getRepType(getPortType(srcPort))
    }

    val isDataPort = dstPort.isInstanceOf[AadlDataPort]
    val isEventPort = dstPort.isInstanceOf[AadlEventPort]
    val isEventDataPort = dstPort.isInstanceOf[AadlEventDataPort]

    val dstQueueSize: Z = if (isDataPort) 1
    else dstPort.asInstanceOf[AadlFeatureEvent].queueSize

    val cTypeName = typeStore.getCTypeName(aadlType)
    val rustTypeName = typeStore.getRustTypeName(aadlType)

    val sharedMemTypeName = QueueTemplate.getTypeQueueTypeName(cTypeName, dstQueueSize)

    val sharedVarName = QueueTemplate.getClientDequeueSharedVarName(dstPort.identifier, dstQueueSize)
    val recvQueueTypeName = QueueTemplate.getClientRecvQueueTypeName(cTypeName, dstQueueSize)
    val recvQueueVarName = QueueTemplate.getClientRecvQueueName(dstPort.identifier)

    var cMethodApiSigs: ISZ[ST] = ISZ()
    var cMethodApis: ISZ[ST] = ISZ()
    var rustExternApis: ISZ[ST] = ISZ()
    var rustUnsafeExternApisWrappers: ISZ[ST] = ISZ()

    if (isEventPort || isEventDataPort) {
      cMethodApiSigs = cMethodApiSigs :+
        QueueTemplate.getClientIsEmpty_C_MethodSig(dstPort.identifier) :+
        QueueTemplate.getClientGetter_C_MethodPollSig(dstPort.identifier, cTypeName, isEventPort) :+
        QueueTemplate.getClientGetter_C_MethodSig(dstPort.identifier, cTypeName, isEventPort)

      cMethodApis = cMethodApis :+
        QueueTemplate.getClientIsEmpty_C_Method(dstPort.identifier, cTypeName, dstQueueSize) :+
        QueueTemplate.getClientGetter_C_MethodPoll(dstPort.identifier, cTypeName, dstQueueSize, isEventPort) :+
        QueueTemplate.getClientGetter_C_Method(dstPort.identifier, cTypeName, isEventPort)

      rustExternApis = rustExternApis :+
        QueueTemplate.getClientIsEmpty_rust_MethodSig(dstPort.identifier, F) :+
        QueueTemplate.getClientGetter_rust_MethodPollSig(dstPort.identifier, rustTypeName, isEventPort, F) :+
        QueueTemplate.getClientGetter_rust_MethodSig(dstPort.identifier, rustTypeName, isEventPort, F)

      rustUnsafeExternApisWrappers = rustUnsafeExternApisWrappers :+
        QueueTemplate.getClientIsEmpty_rust_UnsafeMethod(dstPort.identifier) :+
        QueueTemplate.getClientGetter_rust_UnsafeMethodPoll(dstPort.identifier, rustTypeName, dstQueueSize, isEventPort) :+
        QueueTemplate.getClientGetter_rust_UnsafeMethod(dstPort.identifier, rustTypeName, isEventPort)

    } else {
      cMethodApiSigs = cMethodApiSigs :+
        QueueTemplate.getClientGetter_C_MethodSig(dstPort.identifier, cTypeName, F)

      cMethodApis = cMethodApis :+
        QueueTemplate.getClientDataGetter_C_Method(dstPort.identifier, cTypeName, dstQueueSize, aadlType)

      rustExternApis = rustExternApis :+
        QueueTemplate.getClientGetter_rust_MethodSig(dstPort.identifier, rustTypeName, F, F)

      rustUnsafeExternApisWrappers = rustUnsafeExternApisWrappers :+
        QueueTemplate.getClientDataGetter_rust_UnsafeMethod(dstPort.identifier, rustTypeName, dstQueueSize, aadlType)
    }

    var cUserMethodSignatures: ISZ[ST] = ISZ()
    var cUserMethodDefaultImpls: ISZ[ST] = ISZ()
    var computeContributions: ISZ[ST] = ISZ()
    if (!dstThread.toVirtualMachine(symbolTable)) {
      if (dstThread.isSporadic()) {
        cUserMethodSignatures = cUserMethodSignatures :+ QueueTemplate.getClientEventHandlerMethodSig(dstPort.identifier)

        if (isEventPort || isEventDataPort) {
          computeContributions = computeContributions :+ QueueTemplate.getClientSporadicComputeContributions(dstPort.identifier)
        }

        cUserMethodDefaultImpls = cUserMethodDefaultImpls :+ QueueTemplate.getClientSporadicDefaultImplContributions(dstPort.identifier)
      }
    }

    val memoryRegionName: ISZ[String] = if(srcPort.nonEmpty) srcPort.get.path else dstPort.path

    return UberConnectionContributions(
      portName = dstPort.path,
      portPriority = None(),
      aadlType = aadlType,
      queueSize = dstQueueSize,
      sharedMemoryMapping = ISZ(
        PortSharedMemoryRegion(
          outgoingPortPath = memoryRegionName,
          queueSize = dstQueueSize,
          varAddr = sharedVarName,
          perms = ISZ(Perm.READ),
          sizeInKiBytes = Util.defaultPageSizeInKiBytes,
          physicalAddressInKiBytes = None())
      ),

      cContributions = cConnectionContributions(
        cHeaderImportContributions = ISZ(),
        cImplementationImportContributions = ISZ(),
        cUserMethodSignatures = cUserMethodSignatures,
        cUserMethodDefaultImpls = cUserMethodDefaultImpls,
        cDefineContributions = ISZ(),
        cGlobalVarContributions = ISZ(
          PortVaddr(s"volatile $sharedMemTypeName", s"*$sharedVarName"),
          QueueVaddr(recvQueueTypeName, recvQueueVarName)
        ),
        cApiMethodSigs = cMethodApiSigs,
        cApiMethods = cMethodApis,
        cInitContributions = ISZ(QueueTemplate.getClientRecvInitMethodCall(dstPort.identifier, cTypeName, dstQueueSize)),
        cComputeContributions = computeContributions),

      rustContributions = rustConnectionsContributions(
        rustExternApis = rustExternApis,
        rustUnsafeExternApisWrappers = rustUnsafeExternApisWrappers
      )
    )
  }

  def processOutPort(srcPort: AadlPort,
                     receiverContributions: Map[ISZ[String], UberConnectionContributions],
                     typeStore: TypeStore): UberConnectionContributions = {
    val isEventPort = srcPort.isInstanceOf[AadlEventPort]

    var sharedMemoryMappings: ISZ[MemoryRegion] = ISZ()
    var cSharedMemoryVars: ISZ[GlobalVarContribution] = ISZ()
    var cInitContributions: ISZ[ST] = ISZ()
    var srcPutContributions: ISZ[ST] = ISZ()

    var uniqueQueueSizes: Set[Z] = Set.empty

    val senderPortType: AadlType = typeStore.getRepType(getPortType(srcPort))

    for (receiverContribution <- receiverContributions.values if !uniqueQueueSizes.contains(receiverContribution.queueSize)) {
      uniqueQueueSizes = uniqueQueueSizes + receiverContribution.queueSize

      if (receiverContribution.aadlType != senderPortType) {
        // TODO change to reporter or ensure linter prevents thsi
        halt("Connected ports must have the same type")
      }

      val cTypeName = typeStore.getCTypeName(receiverContribution.aadlType)

      val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, receiverContribution.queueSize)
      srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
        sharedMemoryVarName = sharedMemVarName,
        queueElementTypeName = cTypeName,
        queueSize = receiverContribution.queueSize,
        isEventPort = isEventPort)

      val cPortType: String = typeStore.getCTypeName(receiverContribution.aadlType)
      val queueType = QueueTemplate.getTypeQueueTypeName(cPortType, receiverContribution.queueSize)
      val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, receiverContribution.queueSize)
      cSharedMemoryVars = cSharedMemoryVars :+ QueueVaddr(s"volatile $queueType", s"*$varName")

      cInitContributions = cInitContributions :+ QueueTemplate.getQueueInitMethod(varName, cPortType, receiverContribution.queueSize)

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
      val cTypeName = typeStore.getCTypeName(senderPortType)

      val queueSize = 1

      val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
      srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
        sharedMemoryVarName = sharedMemVarName,
        queueElementTypeName = cTypeName,
        queueSize = queueSize,
        isEventPort = isEventPort)

      val cPortType: String = typeStore.getCTypeName(senderPortType)
      val rustPortType: String = typeStore.getRustTypeName(senderPortType)
      val queueType = QueueTemplate.getTypeQueueTypeName(cPortType, queueSize)
      val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
      cSharedMemoryVars = cSharedMemoryVars :+ QueueVaddr(s"volatile $queueType", s"*$varName")

      cInitContributions = cInitContributions :+ QueueTemplate.getQueueInitMethod(varName, rustPortType, queueSize)

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

    val cPortType: String = typeStore.getCTypeName(senderPortType)
    val rustPortType: String = typeStore.getRustTypeName(senderPortType)
    val cApiMethodSig = QueueTemplate.getClientPut_C_MethodSig(srcPort.identifier, cPortType, isEventPort)
    val cApiMethod = QueueTemplate.getClientPut_C_Method(srcPort.identifier, cPortType, srcPutContributions, isEventPort)

    val rustApiMethodSig = QueueTemplate.getClientPut_rust_MethodSig(srcPort.identifier, rustPortType, isEventPort, F)
    val rustUnsafeExternApisWrappers = QueueTemplate.getClientPut_rust_UnsafeMethod(srcPort.identifier, rustPortType, isEventPort)

    return UberConnectionContributions(
      portName = srcPort.path,
      portPriority = None(),
      aadlType = senderPortType,
      queueSize = 1, // outgoing port so queue size not relevant
      sharedMemoryMapping = sharedMemoryMappings,

      cContributions = cConnectionContributions(
        cHeaderImportContributions = ISZ(),
        cImplementationImportContributions = ISZ(),
        cUserMethodSignatures = ISZ(),
        cUserMethodDefaultImpls = ISZ(),
        cDefineContributions = ISZ(),
        cGlobalVarContributions = cSharedMemoryVars,
        cApiMethodSigs = ISZ(cApiMethodSig),
        cApiMethods = ISZ(cApiMethod),
        cInitContributions = cInitContributions,
        cComputeContributions = ISZ()),

      rustContributions = rustConnectionsContributions(
        rustExternApis = ISZ(rustApiMethodSig),
        rustUnsafeExternApisWrappers = ISZ(rustUnsafeExternApisWrappers)
      )
    )
  }
}
