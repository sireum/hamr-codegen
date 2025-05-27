// #Sireum

package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.common.symbols.AadlPort
import org.sireum.hamr.codegen.common.types.{AadlType, ArrayType}
import org.sireum.hamr.codegen.microkit.plugins.apis.CRustApiUtil
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeNameProvider, CTypeNameProvider, CTypePlugin}
import org.sireum.hamr.codegen.microkit.util.Util.brand
import org.sireum.hamr.codegen.microkit.{rust => RustAst}
import org.sireum.hamr.ir.Direction

object QueueTemplate {

  def getTypeQueueName(queueElementTypeName: String, queueSize: Z): String = {
    return brand(s"queue_${queueElementTypeName}_${queueSize}")
  }

  def getTypeQueueFileName(typeName: String, queueSize: Z): String = {
    return s"${getTypeQueueName(typeName, queueSize)}.h"
  }

  def getTypeQueueTypeName(typeName: String, queueSize: Z): String = {
    return s"${getTypeQueueName(typeName, queueSize)}_t"
  }

  def getTypeRecvQueueName(typeName: String, queueSize: Z): String = {
    return s"${getTypeQueueName(typeName, queueSize)}_Recv"
  }

  def getTypeRecvQueueTypeName(typeName: String, queueSize: Z): String = {
    return s"${getTypeRecvQueueName(typeName, queueSize)}_t"
  }

  def getQueueSizeMacroName(queueName: String): String = {
    return StringUtil.toUpperCase(s"${queueName}_SIZE")
  }

  def getQueueInitMethodName(queueElementTypeName: String,
                             queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_init"
  }

  def getQueueInitMethod(sharedMemoryVar: String,
                         queueElementTypeName: String,
                         queueSize: Z): ST = {
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)
    return st"""${getQueueInitMethodName(queueElementTypeName, queueSize)}(($queueTypeName *) $sharedMemoryVar);"""
  }

  def getQueueEnqueueMethodName(queueElementTypeName: String,
                                queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_enqueue"
  }

  def getQueueDequeueMethodName(queueElementTypeName: String,
                                queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_dequeue"
  }

  def getQueueIsEmptyMethodName(queueElementTypeName: String,
                                queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_is_empty"
  }

  ///// SENDER APIs




  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////
  def getClientPut_C_MethodSig(portName: String,
                               queueElementTypeName: String,
                               isEventPort: B): ST = {
    val param: Option[ST] = if (isEventPort) None() else Some(st"const $queueElementTypeName *data")
    val methodName = st"put_${portName}"
    return st"""bool $methodName($param)"""
  }

  def getClientPut_C_Method(portName: String,
                            queueElementTypeName: String,
                            entries: ISZ[ST],
                            isEventPort: B): ST = {
    val methodSig = getClientPut_C_MethodSig(portName, queueElementTypeName, isEventPort)
    return (
      st"""$methodSig {
          |  ${(entries, "\n\n")}
          |
          |  return true;
          |}""")
  }

  @pure def rustTestingArtifacts(p: AadlPort,
                                 portTypeNameProvider: CRustTypeNameProvider,
                                 aadlType: ISZ[String]): (RustAst.Item, RustAst.Item) = {
    if (p.direction == Direction.In) {
      val varName = s"IN_${p.identifier}"
      val variable = RustAst.ItemStatic(
        ident = RustAst.IdentString(varName),
        visibility = RustAst.Visibility.Public,
        ty = RustAst.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
        mutability = RustAst.Mutability.Not,
        expr = RustAst.ExprST(st"Mutex::new(None);"))
      val test = RustAst.FnImpl(
        attributes = ISZ(RustAst.AttributeST(F, st"cfg(test)")),
        sig = RustAst.FnSig(
          ident = RustAst.IdentString(s"get_${p.identifier}"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(RustAst.ParamImpl(
              ident = RustAst.IdentString("data"),
              kind = RustAst.TyPtr(mutty =
                RustAst.MutTy(
                  ty = RustAst.TyPath(ISZ(portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
                  mutbl = RustAst.Mutability.Mut)))),
            outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F),generics = None()),
        comments = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
          st"""unsafe {
              |  *data = $varName.lock().unwrap().expect("Not expecting None");
              |  return true;
              |}""")))))
      return (variable, test)
    } else {
      val varName = s"OUT_${p.identifier}"
      val variable = RustAst.ItemStatic(
        ident = RustAst.IdentString(varName),
        visibility = RustAst.Visibility.Public,
        ty = RustAst.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
        mutability = RustAst.Mutability.Not,
        expr = RustAst.ExprST(st"Mutex::new(None);"))
      val test = RustAst.FnImpl(
        attributes = ISZ(RustAst.AttributeST(F, st"cfg(test)")),
        sig = RustAst.FnSig(
          ident = RustAst.IdentString(s"put_${p.identifier}"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(RustAst.ParamImpl(
              ident = RustAst.IdentString("data"),
              kind = RustAst.TyPtr(mutty =
                RustAst.MutTy(
                  ty = RustAst.TyPath(ISZ(portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
                  mutbl = RustAst.Mutability.Mut)))),
            outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
          st"""unsafe {
              |  *$varName.lock().unwrap() = Some(*data);
              |  return true;
              |}""")))))
      return (variable, test)
    }

  }

  def rustExternApi_senderMethodSig(methodName: String,
                                    portQualifiedTypeName: ISZ[String],
                                    aadlType: ISZ[String],
                                    isEventPort: B,
                                    unsafe: B): RustAst.FnSig = {
    val args: ISZ[RustAst.Param] =
      if(isEventPort) ISZ()
      else ISZ(
        RustAst.ParamImpl(
          ident = RustAst.IdentString("data"),
          kind = RustAst.TyPtr(
            RustAst.MutTy(
              ty = RustAst.TyPath(ISZ(portQualifiedTypeName), Some(aadlType)),
              mutbl = RustAst.Mutability.Mut))))
    return RustAst.FnSig(
      fnHeader = RustAst.FnHeader(F),
      ident = RustAst.IdentString(methodName),
      verusHeader = None(), generics = None(),
      fnDecl = RustAst.FnDecl(
        inputs = args, outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)))
  }

  def getClientPut_rust_MethodSig(portName: String,
                                  queueElementTypeName: String,
                                  isEventPort: B,
                                  unsafe: B): ST = {
    val param: Option[ST] = if (isEventPort) None() else Some(st"data: *mut $queueElementTypeName")
    val methodName = st"${if(unsafe) "unsafe_" else ""}put_${portName}"
    return st"""pub fn $methodName($param) -> bool"""
  }

  @pure def rustExternApiUnsafeWrapper_UnsafeMethod(portName: String,
                                                    portQualifiedTypeName: ISZ[String],
                                                    aadlType: ISZ[String],
                                                    isEventPort: B,
                                                    unsafe: B): RustAst.Fn = {
    val externMethodName = s"put_${portName}"
    val unsafeMethodName = s"unsafe_$externMethodName"
    val rustType = st"${(portQualifiedTypeName, "::")}"
    val args: ISZ[RustAst.Param] =
      if(isEventPort) ISZ()
      else ISZ(
        RustAst.ParamImpl(
          ident = RustAst.IdentString("data"),
          kind = RustAst.TyRef(
            lifetime = None(),
            mutty = RustAst.MutTy(
              ty = RustAst.TyPath(ISZ(portQualifiedTypeName), Some(aadlType)),
              mutbl = RustAst.Mutability.Not))))
    return RustAst.FnImpl(
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(unsafeMethodName),
        fnDecl = RustAst.FnDecl(inputs = args, outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(),visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
        st"""unsafe {
            |  return $externMethodName(data as *const $rustType as *mut $rustType);
            |}""")))))
  }
  def getClientPut_rust_UnsafeMethod(portName: String,
                                     queueElementTypeName: String,
                                     isEventPort: B): ST = {
    val externMethodName = s"put_$portName"
    val unsafeMethodSig = getClientPut_rust_MethodSig(portName, queueElementTypeName, isEventPort, T)
    return (
      st"""$unsafeMethodSig {
          |  return unsafe { $externMethodName(data) };
          |}""")
  }
  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////


  def getClientPutEntry(sharedMemoryVarName: String,
                        queueElementTypeName: String,
                        queueSize: Z,
                        isEventPort: B): ST = {
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)
    val methodName = getQueueEnqueueMethodName(queueElementTypeName, queueSize)
    val optEvent: Option[ST] = {
      if (isEventPort) Some(st"""${queueElementTypeName} eventPayload = 0; // always send 0 as the event payload
                                |${queueElementTypeName} *data = &eventPayload;""")
      else None()
    }
    return (
      st"""$optEvent
          |$methodName(($queueTypeName *) $sharedMemoryVarName, ($queueElementTypeName *) data);""")
  }

  def getClientEnqueueSharedVarName(portName: String,
                                    queueSize: Z): String = {
    return s"${portName}_queue_${queueSize}"
  }

  ///// RECEIVER APIs

  def getClientRecvQueueTypeName(ququeElementTypeName: String,
                                 queueSize: Z): String = {
    return getTypeRecvQueueTypeName(ququeElementTypeName, queueSize)
  }

  def getClientRecvQueueName(portName: String): String = {
    return s"${portName}_recv_queue"
  }

  def getQueueRecvInitMethodName(queueElementTypeName: String,
                                 queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_Recv_init"
  }

  def getClientRecvInitMethodCall(portName: String,
                                  queueElementTypeName:String,
                                  queueSize: Z): ST = {
    val methodName = getQueueRecvInitMethodName(queueElementTypeName, queueSize)
    val recvQueueVar = getClientRecvQueueName(portName)
    val sharedMemVar = getClientEnqueueSharedVarName(portName, queueSize)
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)
    return st"""${methodName}(&$recvQueueVar, ($queueTypeName *) $sharedMemVar);"""
  }


  def getClientDequeueSharedVarName(portName: String,
                                    queueSize: Z): String = {
    return s"${portName}_queue_${queueSize}"
  }

  def getClientGetterPollMethodName(portName: String): ST = {
    return st"get_${portName}_poll"
  }

  def getClientGetterMethodName(portName: String): ST = {
    return st"get_${portName}"
  }


  //////////////////////////////////////////////////////////////////////////////
  // BEGIN client isEmpty
  //////////////////////////////////////////////////////////////////////////////
  def getClientIsEmptyMethodName(portName: String): ST = {
    return st"${portName}_is_empty"
  }

  def getClientIsEmpty_C_MethodSig(portName: String): ST = {
    return st"bool ${getClientIsEmptyMethodName(portName)}(void)"
  }

  def getClientIsEmpty_C_Method(portName: String,
                                queueElementTypeName: String,
                                queueSize: Z): ST = {
    val methodName = getQueueIsEmptyMethodName(queueElementTypeName, queueSize)
    val recvQueueMemVarName = getClientRecvQueueName(portName)

    return st"""${getClientIsEmpty_C_MethodSig(portName)} {
               |  return ${methodName}(&$recvQueueMemVarName);
               |}"""
  }

  def rustExternApi_getPortMethodIsEmptySig(portName: String): RustAst.FnSig = {
    val methodName = getClientIsEmptyMethodName(portName).render
    return RustAst.FnSig(
      fnHeader = RustAst.FnHeader(F),
      ident = RustAst.IdentString(methodName),
      verusHeader = None(), generics = None(),
      fnDecl = RustAst.FnDecl(inputs = ISZ(), outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)))
  }

  def getClientIsEmpty_rust_MethodSig(portName: String,
                                      unsafe: B): ST = {
    val methodName = getClientIsEmptyMethodName(portName)
    return st"pub fn ${if (unsafe) "unsafe_" else ""}${methodName}() -> bool"
  }

  def rustExternApiUnsafeWrapper_isEmptyMethod(portName: String): RustAst.Fn = {
    val externCMethodName = getClientIsEmptyMethodName(portName).render
    val unsafeMethodName = s"unsafe_$externCMethodName"
    return RustAst.FnImpl(
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(unsafeMethodName),
        fnDecl = RustAst.FnDecl(inputs = ISZ(), outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(),visibility = RustAst.Visibility.Private, contract = None(), meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
        st"""unsafe {
            |  return $externCMethodName();
            |}""")))))
  }

  def getClientIsEmpty_rust_UnsafeMethod(portName: String): ST = {
    val methodName = getClientIsEmptyMethodName(portName)
    val methodSigUnsafe = getClientIsEmpty_rust_MethodSig(portName, T)

    return st"""$methodSigUnsafe {
               |  return unsafe { ${methodName}() };
               |}"""
  }
  //////////////////////////////////////////////////////////////////////////////
  // END client isEmpty
  //////////////////////////////////////////////////////////////////////////////




  //////////////////////////////////////////////////////////////////////////////
  // BEGIN client poll
  //////////////////////////////////////////////////////////////////////////////
  def getClientGetter_C_MethodPollSig(portName: String,
                                      queueElementTypeName: String,
                                      isEventPort: B): ST = {
    val methodName = getClientGetterPollMethodName(portName)
    val optEventData: Option[ST]=
      if (isEventPort) None()
      else Some(st", $queueElementTypeName *data")
    return st"bool $methodName(${MicrokitTypeUtil.eventCounterTypename} *numDropped$optEventData)"
  }

  def getClientGetter_C_MethodPoll(portName: String,
                                   queueElementTypeName: String,
                                   queueSize: Z,
                                   isEventPort: B): ST = {
    val apiMethodName = getQueueDequeueMethodName(queueElementTypeName, queueSize)
    val methodSig = getClientGetter_C_MethodPollSig(portName, queueElementTypeName, isEventPort)
    val recvQueueMemVarName = getClientRecvQueueName(portName)
    val queueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)
    val optEvent: Option[ST] =
      if (isEventPort) Some(
        st"""${queueElementTypeName} eventPortPayload;
            |${queueElementTypeName} *data = &eventPortPayload;""")
      else None()
    return (
      st"""$methodSig {
          |  $optEvent
          |  return $apiMethodName(($queueTypeName *) &$recvQueueMemVarName, numDropped, data);
          |}""")
  }

  def rustExternApi_getPortMethodSig(methodName: String,
                                     qualifiedRustPortTypeName: ISZ[String],
                                     aadlTypeName: ISZ[String],
                                     isEventPort: B): RustAst.FnSig = {
    val args: ISZ[RustAst.Param] =
      if (isEventPort) ISZ()
      else ISZ(
        RustAst.ParamImpl(
          ident = RustAst.IdentString("data"),
          kind = RustAst.TyPtr(
            RustAst.MutTy(
              ty = RustAst.TyPath(ISZ(qualifiedRustPortTypeName), Some(aadlTypeName)),
              mutbl = RustAst.Mutability.Mut))))
    return RustAst.FnSig(
      fnHeader = RustAst.FnHeader(F),
      ident = RustAst.IdentString(methodName),
      verusHeader = None(), generics = None(),
      fnDecl = RustAst.FnDecl(inputs = args, outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)))
  }
  def getClientGetter_rust_MethodPollSig(portName: String,
                                         queueElementTypeName: String,
                                         isEventPort: B,
                                         unsafe: B): ST = {
    val methodName = st"${if (unsafe) "unsafe_" else ""}${getClientGetterPollMethodName(portName)}"
    val optEventData: Option[ST]=
      if (isEventPort) None()
      else Some(st", data: *mut $queueElementTypeName")
    return st"pub fn $methodName(num_dropped: *mut ${MicrokitTypeUtil.eventCounterTypename}$optEventData)"
  }

  def rustExternApiUnsafeWrapper_poll(portName: String,
                                      rustPortTypeName: ISZ[String],
                                      aadlTypeName: ISZ[String],
                                      examplePortTypeData: String,
                                      isEventPort: B): RustAst.Fn = {

    val externCMethodName = getClientGetterPollMethodName(portName)
    val unsafeMethodName = st"unsafe_$externCMethodName"

    val returnType: RustAst.Ty =
      if (isEventPort) RustAst.TypeRust(ISZ(MicrokitTypeUtil.eventCounterTypename))
      else RustAst.TyTuple(ISZ(
        RustAst.TyPath(ISZ(ISZ(MicrokitTypeUtil.eventCounterTypename)), None()),
        RustAst.TyPath(ISZ(rustPortTypeName), Some(aadlTypeName))
      ))

    val qualTypeName = st"${(rustPortTypeName, "::")}"
    val body: ST =
      if (isEventPort)
        st"""let mut numDropped: *mut ${MicrokitTypeUtil.eventCounterTypename} = &mut 0;
            |$externCMethodName(numDropped);
            |return *numDropped;"""
      else
        st"""let numDropped: *mut ${MicrokitTypeUtil.eventCounterTypename} = &mut 0;
            |let data: *mut $qualTypeName = &mut $examplePortTypeData;
            |$externCMethodName(numDropped, data);
            |return (*numDropped, *data);"""

    return RustAst.FnImpl(
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(unsafeMethodName.render),
        fnDecl = RustAst.FnDecl(inputs = ISZ(), outputs = RustAst.FnRetTyImpl(returnType)),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
        st"""unsafe {
            |  $body
            |}""")))))
  }

  def getClientGetter_rust_UnsafeMethodPoll(portName: String,
                                            queueElementTypeName: String,
                                            queueSize: Z,
                                            isEventPort: B): ST = {
    val methodName = getClientGetterPollMethodName(portName)
    val methodSigUnsafe = getClientGetter_rust_MethodPollSig(portName, queueElementTypeName, isEventPort, T)
    return (
      st"""$methodSigUnsafe {
          |  return unsafe { $methodName(num_dropped, data) };
          |}""")
  }
  //////////////////////////////////////////////////////////////////////////////
  // END client poll
  //////////////////////////////////////////////////////////////////////////////



  def getClientDataGetter_C_Method(portName: String,
                                   queueElementTypeName: String,
                                   queueSize: Z,
                                   aadlType: AadlType,
                                   cTypeNameProvider: CTypeNameProvider): ST = {
    val apiMethodName = getQueueDequeueMethodName(queueElementTypeName, queueSize)
    val methodSig = getClientGetter_C_MethodSig(portName, queueElementTypeName, F)
    val recvQueueMemVarName = getClientRecvQueueName(portName)
    val queueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)

    val lastName = s"last_${portName}_payload"

    val (copyInfra, copyUser): (ST, ST) = aadlType match {
      case a: ArrayType => (
        st"memcpy(&$lastName, &fresh_data, ${CTypePlugin.getArrayStringByteSizeDefineName(cTypeNameProvider)})",
        st"memcpy(data, &$lastName, ${CTypePlugin.getArrayStringByteSizeDefineName(cTypeNameProvider)})")
      case _ => (
        st"$lastName = fresh_data",
        st"*data = $lastName")
    }

    return (
      st"""${queueElementTypeName} $lastName;
          |
          |$methodSig {
          |  ${MicrokitTypeUtil.eventCounterTypename} numDropped;
          |  ${queueElementTypeName} fresh_data;
          |  bool isFresh = $apiMethodName(($queueTypeName *) &$recvQueueMemVarName, &numDropped, &fresh_data);
          |  if (isFresh) {
          |    $copyInfra;
          |  }
          |  $copyUser;
          |  return isFresh;
          |}""")
  }

  def getClientDataGetter_rust_UnsafeMethod(portName: String,
                                            queueElementTypeName: String,
                                            queueSize: Z,
                                            aadlType: AadlType): ST = {
    val externMethodName = getClientGetterMethodName(portName)
    val unsafeMethodSig = getClientGetter_rust_MethodSig(
      portName = portName, queueElementTypeName = queueElementTypeName, isEventPort = F, unsafe = T)
    return (
      st"""$unsafeMethodSig {
          |  return unsafe { $externMethodName(data) };
          |}""")
  }



  /////////////////////////////////////////////////////////////////////////////////////////////
  // BEGIN client getter
  /////////////////////////////////////////////////////////////////////////////////////////////

  def getClientGetter_C_MethodSig(portName: String,
                                  queueElementTypeName: String,
                                  isEventPort: B): ST = {
    val methodName = getClientGetterMethodName(portName)
    val optEventDataPort: Option[ST] =
      if (isEventPort) None()
      else Some(st"$queueElementTypeName *data")
    return st"bool $methodName($optEventDataPort)"
  }

  def getClientGetter_C_Method(portName: String,
                               queueElementTypeName: String,
                               isEventPort: B): ST = {
    val methodSig = getClientGetter_C_MethodSig(portName, queueElementTypeName, isEventPort)
    val methodName = getClientGetterPollMethodName(portName)
    val optEventData: Option[ST] =
      if(isEventPort) None() else Some(st", data")
    return (
      st"""$methodSig {
          |  ${MicrokitTypeUtil.eventCounterTypename} numDropped;
          |  return $methodName (&numDropped$optEventData);
          |}""")
  }

  def getClientGetter_rust_MethodSig(portName: String,
                                     queueElementTypeName: String,
                                     isEventPort: B,
                                     unsafe: B): ST = {
    val methodName = getClientGetterMethodName(portName)
    val optEventDataPort: Option[ST] =
      if (isEventPort) None()
      else Some(st"data: *mut $queueElementTypeName")
    return st"pub fn ${if (unsafe) "unsafe_" else ""}$methodName($optEventDataPort) -> bool"
  }

  def rustExternApiUnsafeWrapper_getMethod(portName: String,
                                           rustPortTypeName: ISZ[String],
                                           aadlTypeName: ISZ[String],
                                           examplePortTypeData: String,
                                           isEventPort: B): RustAst.Fn = {
    val externCMethodName = getClientGetterMethodName(portName)
    val unsafeMethodName = st"unsafe_$externCMethodName"
    val qualPortTypeName = st"${(rustPortTypeName, "::")}"
    val body: RustAst.MethodBody =
      if (isEventPort)
        RustAst.MethodBody(ISZ(RustAst.BodyItemST(
          st"""unsafe {
              |  return $externCMethodName();
              |}""")))
      else
        RustAst.MethodBody(ISZ(RustAst.BodyItemST(
          st"""unsafe {
              |  let data: *mut $qualPortTypeName = &mut $examplePortTypeData;
              |  $externCMethodName(data);
              |  return *data;
              |}""")))
    val returnType: RustAst.Ty =
      if (isEventPort) MicrokitTypeUtil.rustBoolType
      else RustAst.TyPath(ISZ(rustPortTypeName), Some(aadlTypeName))
    return RustAst.FnImpl(
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(unsafeMethodName.render),
        fnDecl = RustAst.FnDecl(inputs = ISZ(), outputs = RustAst.FnRetTyImpl(returnType)),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F),generics = None()),
      comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
      body = Some(body))
  }

  def getClientGetter_rust_UnsafeMethod(portName: String,
                                        queueElementTypeName: String,
                                        isEventPort: B): ST = {
    val methodName = getClientGetterMethodName(portName)
    val methodSigUnsafe = getClientGetter_rust_MethodSig(portName, queueElementTypeName, isEventPort, T)
    val optEventData: Option[ST] =
      if(isEventPort) None() else Some(st"data")
    return (
      st"""$methodSigUnsafe {
          |  return unsafe { $methodName($optEventData) };
          |}""")
  }
  /////////////////////////////////////////////////////////////////////////////////////////////
  // END client getter
  /////////////////////////////////////////////////////////////////////////////////////////////


  def getClientEventHandlerMethodName(portName: String): ST = {
    return st"handle_$portName"
  }

  def getClientEventHandlerMethodSig(portName: String): ST = {
    return st"""void ${getClientEventHandlerMethodName(portName)}(void)"""
  }

  def getClientSporadicComputeContributions(portName: String): ST = {
    val userMethodName = s"handle_${portName}"
    val isEmpty = getClientIsEmptyMethodName(portName)
    return (
      st"""if (!$isEmpty()) {
          |  ${userMethodName}();
          |}""")
  }

  def getClientSporadicDefaultImplContributions(portName: String): ST = {
    return (
      st"""void handle_$portName(void) {
          |  printf("%s: handel_$portName invoked\n", microkit_name);
          |}""")
  }

  def header(queueElementTypeName: String,
             queueSize: Z): ST = {

    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)

    val recvQueueName = getTypeRecvQueueName(queueElementTypeName, queueSize)
    val recvQueueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)

    val queueSizeMacroName = getQueueSizeMacroName(queueName)

    val r =
      st"""/*
          | * Copyright 2017, Data61
          | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
          | * ABN 41 687 119 230.
          | *
          | * Copyright 2019 Adventium Labs
          | * Modifications made to original
          | *
          | * This software may be distributed and modified according to the terms of
          | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
          | * See "LICENSE_BSD2.txt" for details.
          | *
          | * @TAG(DATA61_Adventium_BSD)
          | */
          |
          |// Single sender multiple receiver Queue implementation for AADL
          |// Ports. Every receiver receives the sent data (ie broadcast). The queue
          |// operations are all non-blocking. The sender enqueue always succeeds. A
          |// receiver dequeue can fail and drop data if the sender writes while the
          |// receiver is reading. This situation is detected unless the sender gets
          |// ahead of a receiver by more than COUNTER_MAX. Since COUNTER_MAX is typically
          |// 2^64 (see ${MicrokitTypeUtil.cEventCounterFilename}), this is extremely unlikely. If it does happen the
          |// only adverse effect is that the receiver will not detect all dropped
          |// elements.
          |
          |#pragma once
          |
          |#include <${MicrokitTypeUtil.cEventCounterFilename}>
          |#include <${MicrokitTypeUtil.cAadlTypesFilename}>
          |
          |#include <stdbool.h>
          |#include <stddef.h>
          |#include <stdint.h>
          |
          |#if __has_include("util.h")
          |#include <util.h>
          |#elif __has_include("libvmm/util.util.h")
          |#include <libvmm/util/util.h>
          |#endif
          |
          |// Queue size must be an integer factor of the size for ${MicrokitTypeUtil.eventCounterTypename} (an unsigned
          |// integer type). Since we are using standard C unsigned integers for the
          |// counter, picking a queue size that is a power of 2 is a good choice. We
          |// could alternatively set the size of our counter to the largest possible
          |// multiple of queue size. But then we would need to do our own modulo
          |// operations on the counter rather than depending on c's unsigned integer
          |// operations.
          |//
          |// Note: One cell in the queue is always considered dirty. Its the next
          |// element to be written. Thus the queue can only contain
          |// ${queueSizeMacroName}-1 elements.
          |#define ${queueSizeMacroName} ${queueSize + 1}
          |
          |// This is the type of the seL4 dataport (shared memory) that is shared by the
          |// sender and all receivers. This type is referenced in the sender and receiver
          |// component definition files. The seL4 runtime creates an
          |// instance of this struct.
          |typedef struct ${queueName} {
          |  // Number of elements enqueued by the sender. The implementation depends
          |  // on C's standard module behaviour for unsigned integers. The counter never
          |  // overflows. It just wraps modulo the size of the counter type. The counter
          |  // is typically very large (see ${MicrokitTypeUtil.cEventCounterFilename}), so this should happen very
          |  // infrequently. Depending in C to initialize this to zero.
          |  _Atomic ${MicrokitTypeUtil.eventCounterTypename} numSent;
          |
          |  // Queue of elements of type ${queueElementTypeName}
          |  // (see ${MicrokitTypeUtil.cTypesFilename}) implemented as a ring buffer.
          |  // No initialization necessary.
          |  ${queueElementTypeName} elt[${queueSizeMacroName}];
          |
          |} ${queueTypeName};
          |
          |//------------------------------------------------------------------------------
          |// Sender API
          |//
          |// Could split this into separate header and source file since only sender
          |// code needs this.
          |
          |// Initialize the queue. Sender must call this exactly once before any calls to queue_enqueue();
          |void ${queueName}_init(${queueTypeName} *queue);
          |
          |// Enqueue data. This always succeeds and never blocks. Data is copied.
          |void ${queueName}_enqueue(
          |  ${queueTypeName} *queue,
          |  ${queueElementTypeName} *data);
          |
          |//------------------------------------------------------------------------------
          |// Receiver API
          |//
          |// Could split this into separate header and source file since only receiver
          |// code needs this.
          |
          |// Each receiver needs to create an instance of this.
          |typedef struct ${recvQueueName} {
          |  // Number of elements dequeued (or dropped) by a receiver. The implementation
          |  // depends on C's standard module behaviour for unsigned integers. The
          |  // counter never overflows. It just wraps modulo the size of the counter
          |  // type. The counter is typically very large (see counter.h), so this should
          |  // happen very infrequently.
          |  ${MicrokitTypeUtil.eventCounterTypename} numRecv;
          |
          |  // Pointer to the actual queue. This is the seL4 dataport (shared memory)
          |  // that is shared by the sender and all receivers.
          |  ${queueTypeName} *queue;
          |
          |} ${recvQueueTypeName};
          |
          |// Each receiver must call this exactly once before any calls to other queue
          |// API functions.
          |void ${recvQueueName}_init(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${queueTypeName} *queue);
          |
          |// Dequeue data. Never blocks but can fail if the sender writes at same
          |// time.
          |
          |// When successful returns true. The dequeued data will be copied to
          |// *data. *numDropped will contain the number of elements that were dropped
          |// since the last call to queue_dequeue().
          |//
          |// When queue is empty, returns false and *numDropped is zero. *data is left in
          |// unspecified state.
          |//
          |// When dequeue fails due to possible write of data being read, returns false
          |// and *numDropped will be >= 1 specifying the number of elements that were
          |// dropped since the last call to ${queueName}_dequeue(). *data is left in
          |// unspecified state.
          |//
          |// If the sender ever gets ahead of a receiver by more than COUNTER_MAX,
          |// ${queueName}_dequeue will fail to count a multiple of COUNTER_MAX in
          |// numDropped. Since COUNTER_MAX is very large (typically on the order of 2^64,
          |// see ${MicrokitTypeUtil.cEventCounterFilename}), this is very unlikely.  If the sender is ever this far
          |// ahead of a receiver the system is probably in a very bad state.
          |bool ${queueName}_dequeue(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${MicrokitTypeUtil.eventCounterTypename} *numDropped,
          |  ${queueElementTypeName} *data);
          |
          |// Is queue empty? If the queue is not empty, it will stay that way until the
          |// receiver dequeues all data. If the queue is empty you can make no
          |// assumptions about how long it will stay empty.
          |bool ${queueName}_is_empty(${recvQueueTypeName} *recvQueue);
          |"""
    return r
  }

  def implementation(aadlType: AadlType,
                     queueElementTypeName: String,
                     queueSize: Z,
                     cTypeNameProvider: CTypeNameProvider): ST = {

    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)

    val queueHeaderFilename = getTypeQueueFileName(queueElementTypeName, queueSize)

    val recvQueueName = getTypeRecvQueueName(queueElementTypeName, queueSize)
    val recvQueueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)
    val queueSizeMacroName = getQueueSizeMacroName(queueName)

    val initMethodName = getQueueInitMethodName(queueElementTypeName, queueSize)
    val recvInitMethodName = getQueueRecvInitMethodName(queueElementTypeName, queueSize)
    val dequeueMethodName = getQueueDequeueMethodName(queueElementTypeName, queueSize)
    val enqueueMethodName = getQueueEnqueueMethodName(queueElementTypeName, queueSize)
    val isEmptyMethodName = getQueueIsEmptyMethodName(queueElementTypeName, queueSize)

    val enqueue: ST = {
      aadlType match {
        case a: ArrayType => st"memcpy(&queue->elt[index], data, ${CTypePlugin.getArrayStringByteSizeDefineName(cTypeNameProvider)})"
        case _ => st"queue->elt[index] = *data"
      }
    }

    val dequeue: ST = {
      aadlType match {
        case a: ArrayType => st"memcpy(data, &queue->elt[index], ${CTypePlugin.getArrayStringByteSizeDefineName(cTypeNameProvider)})"
        case _ => st"*data = queue->elt[index]"
      }
    }

    val r =
      st"""/*
          | * Copyright 2017, Data61
          | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
          | * ABN 41 687 119 230.
          | *
          | * Copyright 2019 Adventium Labs
          | * Modifications made to original
          | *
          | * This software may be distributed and modified according to the terms of
          | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
          | * See "LICENSE_BSD2.txt" for details.
          | *
          | * @TAG(DATA61_Adventium_BSD)
          | */
          |
          |#include <${queueHeaderFilename}>
          |
          |//------------------------------------------------------------------------------
          |// Sender API
          |//
          |// See ${queueHeaderFilename} for API documentation. Only implementation details are documented here.
          |
          |void ${initMethodName}(${queueTypeName} *queue) {
          |  // NOOP for now. C's struct initialization is sufficient.  If we ever do need
          |  // initialization logic, we may also need to synchronize with receiver
          |  // startup.
          |}
          |
          |void ${enqueueMethodName}(
          |  ${queueTypeName} *queue,
          |  ${queueElementTypeName} *data) {
          |
          |  // Simple ring with one dirty element that will be written next. Only one
          |  // writer, so no need for any synchronization.
          |  // elt[queue->numSent % ${queueSizeMacroName}]
          |  // is always considered dirty. So do not advance queue->NumSent
          |  // till AFTER data is copied.
          |
          |  size_t index = queue->numSent % ${queueSizeMacroName};
          |
          |  $enqueue; // Copy data into queue
          |
          |  // Release memory fence - ensure that data write above completes BEFORE we advance queue->numSent
          |  __atomic_thread_fence(__ATOMIC_RELEASE);
          |
          |  ++(queue->numSent);
          |}
          |
          |//------------------------------------------------------------------------------
          |// Receiver API
          |//
          |// See ${queueHeaderFilename} for API documentation. Only implementation details are documented here.
          |
          |void ${recvInitMethodName}(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${queueTypeName} *queue) {
          |
          |  recvQueue->numRecv = 0;
          |  recvQueue->queue = queue;
          |}
          |
          |bool ${dequeueMethodName}(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${MicrokitTypeUtil.eventCounterTypename} *numDropped,
          |  ${queueElementTypeName} *data) {
          |
          |  ${MicrokitTypeUtil.eventCounterTypename} *numRecv = &recvQueue->numRecv;
          |  ${queueTypeName} *queue = recvQueue->queue;
          |
          |  // Get a copy of numSent so we can see if it changes during read
          |  ${MicrokitTypeUtil.eventCounterTypename} numSent = queue->numSent;
          |
          |  // Acquire memory fence - ensure read of queue->numSent BEFORE reading data
          |  __atomic_thread_fence(__ATOMIC_ACQUIRE);
          |
          |  // How many new elements have been sent? Since we are using unsigned
          |  // integers, this correctly computes the value as counters wrap.
          |  ${MicrokitTypeUtil.eventCounterTypename} numNew = numSent - *numRecv;
          |  if (0 == numNew) {
          |    // Queue is empty
          |    return false;
          |  }
          |
          |  // One element in the ring buffer is always considered dirty. Its the next
          |  // element we will write.  It's not safe to read it until numSent has been
          |  // incremented. Thus there are really only (${queueSizeMacroName} - 1)
          |  // elements in the queue.
          |  *numDropped = (numNew <= ${queueSizeMacroName} - 1) ? 0 : numNew - ${queueSizeMacroName} + 1;
          |
          |  // Increment numRecv by *numDropped plus one for the element we are about to read.
          |  *numRecv += *numDropped + 1;
          |
          |  // UNUSED - number of elements left to be consumed
          |  //${MicrokitTypeUtil.eventCounterTypename} numRemaining = numSent - *numRecv;
          |
          |  size_t index = (*numRecv - 1) % ${queueSizeMacroName};
          |  $dequeue; // Copy data
          |
          |  // Acquire memory fence - ensure read of data BEFORE reading queue->numSent again
          |  __atomic_thread_fence(__ATOMIC_ACQUIRE);
          |
          |  if (queue->numSent - *numRecv + 1 < ${queueSizeMacroName}) {
          |    // Sender did not write element we were reading. Copied data is coherent.
          |    return true;
          |  } else {
          |    // Sender may have written element we were reading. Copied data may be incoherent.
          |    // We dropped the element we were trying to read, so increment *numDropped.
          |    ++(*numDropped);
          |    return false;
          |  }
          |}
          |
          |bool ${isEmptyMethodName}(${recvQueueTypeName} *recvQueue) {
          |  return (recvQueue->queue->numSent == recvQueue->numRecv);
          |}"""
    return r
  }
}