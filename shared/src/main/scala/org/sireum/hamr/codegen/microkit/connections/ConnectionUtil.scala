// #Sireum
package org.sireum.hamr.codegen.microkit.connections

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureEvent, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeProvider, CTypeProvider}
import org.sireum.hamr.codegen.microkit.types.{MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.codegen.microkit.util._
import org.sireum.hamr.ir.{ConnectionInstance, Name}

object ConnectionUtil {

  def processInPort(dstThread: AadlThread,
                    dstPort: AadlPort,
                    srcPort: Option[AadlPort],
                    cTypeProvider: CTypeProvider,
                    symbolTable: SymbolTable): UberConnectionContributions = {

    // linters should have guaranteed that if srcPort is populated then it must have the same
    // type as dstPort
    val aadlType = cTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(dstPort))

    val isDataPort = dstPort.isInstanceOf[AadlDataPort]
    val isEventPort = dstPort.isInstanceOf[AadlEventPort]
    val isEventDataPort = dstPort.isInstanceOf[AadlEventDataPort]

    val dstQueueSize: Z = if (isDataPort) 1
    else dstPort.asInstanceOf[AadlFeatureEvent].queueSize

    val cTypeNameProvider = cTypeProvider.getTypeNameProvider(aadlType)

    val pageSize: Z =
      aadlType.bitSize match {
        case Some(bits) =>
          val p = Util.bytesToKiBytes(Util.bitsToBytes(bits))
          if (p < Util.defaultPageSizeInKiBytes) Util.defaultPageSizeInKiBytes
          else p
        case _ => Util.defaultPageSizeInKiBytes
      }

    val cTypeName = cTypeNameProvider.mangledName

    val sharedMemTypeName = QueueTemplate.getTypeQueueTypeName(cTypeName, dstQueueSize)

    val sharedVarName = QueueTemplate.getClientDequeueSharedVarName(dstPort.identifier, dstQueueSize)
    val recvQueueTypeName = QueueTemplate.getClientRecvQueueTypeName(cTypeName, dstQueueSize)
    val recvQueueVarName = QueueTemplate.getClientRecvQueueName(dstPort.identifier)

    var cMethodApiSigs: ISZ[ST] = ISZ()
    var cMethodApis: ISZ[ST] = ISZ()

    if (isEventPort || isEventDataPort) {
      cMethodApiSigs = cMethodApiSigs :+
        QueueTemplate.getClientIsEmpty_C_MethodSig(dstPort.identifier) :+
        QueueTemplate.getClientGetter_C_MethodPollSig(dstPort.identifier, cTypeName, isEventPort) :+
        QueueTemplate.getClientGetter_C_MethodSig(dstPort.identifier, cTypeName, isEventPort)

      cMethodApis = cMethodApis :+
        QueueTemplate.getClientIsEmpty_C_Method(dstPort.identifier, cTypeName, dstQueueSize) :+
        QueueTemplate.getClientGetter_C_MethodPoll(dstPort.identifier, cTypeName, dstQueueSize, isEventPort) :+
        QueueTemplate.getClientGetter_C_Method(dstPort.identifier, cTypeName, isEventPort)
    } else {
      cMethodApiSigs = cMethodApiSigs :+
        QueueTemplate.getClientGetter_C_MethodSig(dstPort.identifier, cTypeName, F)

      cMethodApis = cMethodApis :+
        QueueTemplate.getClientDataGetter_C_Method(dstPort.identifier, cTypeName, dstQueueSize, aadlType, cTypeNameProvider)
    }

    var cEntrypointMethodSignatures: ISZ[ST] = ISZ()
    var cUserMethodDefaultImpls: ISZ[ST] = ISZ()
    var computeContributions: ISZ[ST] = ISZ()
    if (!dstThread.toVirtualMachine(symbolTable)) {
      if (dstThread.isSporadic()) {
        cEntrypointMethodSignatures = cEntrypointMethodSignatures :+ QueueTemplate.getClientEventHandlerMethodSig(dstPort.identifier)

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
          sizeInKiBytes = pageSize,
          physicalAddressInKiBytes = None())
      ),

      cContributions = cConnectionContributions(
        cBridge_EntrypointMethodSignatures = cEntrypointMethodSignatures,
        cUser_MethodDefaultImpls = cUserMethodDefaultImpls,
        cBridge_GlobalVarContributions = ISZ(
          PortVaddr(s"volatile $sharedMemTypeName", s"*$sharedVarName"),
          QueueVaddr(recvQueueTypeName, recvQueueVarName)
        ),
        cPortApiMethodSigs = cMethodApiSigs,
        cBridge_PortApiMethods = cMethodApis,
        cBridge_InitContributions = ISZ(QueueTemplate.getClientRecvInitMethodCall(dstPort.identifier, cTypeName, dstQueueSize)),
        cBridge_ComputeContributions = computeContributions)
    )
  }

  def processOutPort(srcPort: AadlPort,
                     receiverContributions: Map[ISZ[String], UberConnectionContributions],
                     cTypeProvider: CTypeProvider): UberConnectionContributions = {
    val isEventPort = srcPort.isInstanceOf[AadlEventPort]

    var sharedMemoryMappings: ISZ[MemoryRegion] = ISZ()
    var cSharedMemoryVars: ISZ[GlobalVarContribution] = ISZ()
    var cInitContributions: ISZ[ST] = ISZ()
    var srcPutContributions: ISZ[ST] = ISZ()

    var uniqueQueueSizes: Set[Z] = Set.empty

    val senderPortType: AadlType = cTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(srcPort))

    for (receiverContribution <- receiverContributions.values if !uniqueQueueSizes.contains(receiverContribution.queueSize)) {
      uniqueQueueSizes = uniqueQueueSizes + receiverContribution.queueSize

      if (receiverContribution.aadlType != senderPortType) {
        // TODO change to reporter or ensure linter prevents thsi
        halt("Connected ports must have the same type")
      }

      val cTypeName = cTypeProvider.getTypeNameProvider(receiverContribution.aadlType).mangledName

      val pageSize: Z =
        receiverContribution.aadlType.bitSize match {
          case Some(bits) =>
            val p = Util.bytesToKiBytes(Util.bitsToBytes(bits))
            if (p < Util.defaultPageSizeInKiBytes) Util.defaultPageSizeInKiBytes
            else p
          case _ => Util.defaultPageSizeInKiBytes
        }

      val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, receiverContribution.queueSize)
      srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
        sharedMemoryVarName = sharedMemVarName,
        queueElementTypeName = cTypeName,
        queueSize = receiverContribution.queueSize,
        isEventPort = isEventPort)

      val cPortType: String = cTypeProvider.getTypeNameProvider(receiverContribution.aadlType).mangledName
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
          sizeInKiBytes = pageSize,
          physicalAddressInKiBytes = None()
        )
    }

    if (receiverContributions.isEmpty) {
      val cTypeName = cTypeProvider.getTypeNameProvider(senderPortType).mangledName

      val pageSize: Z =
        senderPortType.bitSize match {
          case Some(bits) =>
            val p = Util.bytesToKiBytes(Util.bitsToBytes(bits))
            if (p < Util.defaultPageSizeInKiBytes) Util.defaultPageSizeInKiBytes
            else p
          case _ => Util.defaultPageSizeInKiBytes
        }

      val queueSize = 1

      val sharedMemVarName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
      srcPutContributions = srcPutContributions :+ QueueTemplate.getClientPutEntry(
        sharedMemoryVarName = sharedMemVarName,
        queueElementTypeName = cTypeName,
        queueSize = queueSize,
        isEventPort = isEventPort)

      val cPortType: String = cTypeProvider.getTypeNameProvider(senderPortType).mangledName
      val queueType = QueueTemplate.getTypeQueueTypeName(cPortType, queueSize)
      val varName = QueueTemplate.getClientEnqueueSharedVarName(srcPort.identifier, queueSize)
      cSharedMemoryVars = cSharedMemoryVars :+ QueueVaddr(s"volatile $queueType", s"*$varName")

      cInitContributions = cInitContributions :+ QueueTemplate.getQueueInitMethod(varName, cPortType, queueSize)

      sharedMemoryMappings = sharedMemoryMappings :+
        PortSharedMemoryRegion(
          outgoingPortPath = srcPort.path,
          queueSize = queueSize,
          varAddr = varName,
          perms = ISZ(Perm.READ, Perm.WRITE),
          sizeInKiBytes = pageSize,
          physicalAddressInKiBytes = None()
        )
    }

    val cPortType: String = cTypeProvider.getTypeNameProvider(senderPortType).mangledName
    val cPortApiMethodSig = QueueTemplate.getClientPut_C_MethodSig(srcPort.identifier, cPortType, isEventPort)
    val cApiMethod = QueueTemplate.getClientPut_C_Method(srcPort.identifier, cPortType, srcPutContributions, isEventPort)

    return UberConnectionContributions(
      portName = srcPort.path,
      portPriority = None(),
      aadlType = senderPortType,
      queueSize = 1, // outgoing port so queue size not relevant
      sharedMemoryMapping = sharedMemoryMappings,

      cContributions = cConnectionContributions(
        cBridge_EntrypointMethodSignatures = ISZ(),
        cUser_MethodDefaultImpls = ISZ(),
        cBridge_GlobalVarContributions = cSharedMemoryVars,
        cPortApiMethodSigs = ISZ(cPortApiMethodSig),
        cBridge_PortApiMethods = ISZ(cApiMethod),
        cBridge_InitContributions = cInitContributions,
        cBridge_ComputeContributions = ISZ())
    )
  }
}
