// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.Port
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.templates.{CommentTemplate, StackFrameTemplate}
import org.sireum.hamr.codegen.common.types.{TypeKind, TypeNameProvider, TypeResolver, TypeUtil}
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider

object SeL4NixTemplate {

  val TRANSPILER_TOUCHER_OBJECT_NAME: String = "TranspilerToucher"
  val TRANSPILER_TOUCHER_METHOD_NAME: String = "touch"

  def sendOutput(entries: ST): ST = {
    val ret: ST =
      st"""def sendOutput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
          |  // ignore params
          |
          |  ${entries}
          |}"""
    return ret
  }

  def getValue(entries: ST): ST = {
    val ret: ST =
      st"""def getValue(portId: Art.PortId): Option[DataContent] = {
          |  ${entries}
          |}"""
    return ret

  }

  def putValue(entries: ST): ST = {
    val ret: ST =
      st"""def putValue(portId: Art.PortId, data: DataContent): Unit = {
          |  ${entries}
          |}"""
    return ret
  }

  def receiveInput(entries: ST): ST = {
    val ret: ST =
      st"""def receiveInput(eventPortIds: ISZ[Art.PortId], dataPortIds: ISZ[Art.PortId]): Unit = {
          |  // ignore params
          |
          |  ${entries}
          |}"""
    return ret
  }

  def portComment(portName: String,
                  dir: String,
                  portCategory: String,
                  portType: String): ST = {
    return st"${portName}: ${dir} ${portCategory} ${portType}"
  }

  def portVariable(bridgeIdentifier: String,
                   portVariableName: String,
                   archPortName: String,
                   portId: String,
                   portComment: ST): ST = {
    val ret: ST =
      st"""// ${portComment}
          |val ${portId}: Art.PortId = ${bridgeIdentifier}.${archPortName}.id
          |var ${portVariableName}: Option[DataContent] = noData"""
    return ret
  }

  def extensionObjectStub(packageName: String,
                          sel4ExtensionStubName: String,
                          entries: ST): ST = {
    val ret: ST =
      st"""package ${packageName}
          |
          |import org.sireum._
          |import art._
          |
          |object ${sel4ExtensionStubName} {
          |  ${entries}
          |}
          |"""
    return ret
  }

  def extensionObject(packageName: String,
                      sel4ExtensionName: String,
                      entries: ST): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |package ${packageName}
          |
          |import org.sireum._
          |import art._
          |
          |@ext object ${sel4ExtensionName} {
          |  ${entries}
          |}
          |"""
    return ret
  }

  def dispatchStatus(body: ST): ST = {
    val ret: ST =
      st"""def dispatchStatus(bridgeId: Art.BridgeId): DispatchStatus = {
          |  ${body}
          |}"""
    return ret
  }

  @pure def portApiUsage(initApi: String, operApi: String, p: Port): ST = {
    if (CommonUtil.isInFeature(p.feature)) {
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      return st"val apiUsage_${p.name}: Option[${typeName}] = ${operApi}.get.get_${p.name}()"
    } else {
      val payload: String =
        if (p.getPortTypeNames.isEmptyType) ""
        else p.getPortTypeNames.example()
      return (
        st"""${initApi}.get.put_${p.name}($payload)
            |${operApi}.get.put_${p.name}($payload)""")
    }
  }

  def apiTouches(names: NameProvider, ports: ISZ[Port]): ISZ[ST] = {
    var ret: ISZ[ST] = ISZ()
    val apis = ISZ(names.cApiInitialization_Id, names.cApiOperational_Id)
      .map((m: String) => s"${names.packageName}.${names.bridge}.${m}")
    val loggers = ISZ("logInfo", "logDebug", "logError")
    for (api <- apis) {
      for (logger <- loggers) {
        ret = ret :+ st"""${api}.get.${logger}("")"""
      }
    }
    for (port <- ports) {
      ret = ret :+ portApiUsage(apis(0), apis(1), port)
    }
    return ret
  }

  def touchType(payloadName: String, typeName: Option[String]): ST = {
    return st"printDataContent(${payloadName}(${typeName}))"
  }

  def touchTypes(touches: ISZ[ST]): ST = {
    val ret: ST =
      st"""// touch each payload/type in case some are only used as a field in a record
          |def printDataContent(a: art.DataContent): Unit = { println(s"$${a}") }
          |
          |${(touches, "\n")}"""
    return ret
  }

  def genTouchMethod(typeTouches: ISZ[ST], apiTouches: ISZ[ST], scheduleTouches: ISZ[ST]): ST = {
    val sts: Option[ST] =
      if (scheduleTouches.nonEmpty) Some(
        st"""// touch process/thread timing properties
            |${(scheduleTouches, "\n")}
                                            """)
      else None()

    val ret: ST =
      st"""def touch(): Unit = {
          |  if(F) {
          |    ${callTranspilerToucher()}
          |
          |    // add types used in Platform.receive and Platform.receiveAsync
          |    val mbox2Boolean_Payload: MBox2[Art.PortId, DataContent] = MBox2(portId"0", Base_Types.Boolean_Payload(T))
          |    val mbox2OptionDataContent: MBox2[Art.PortId, Option[DataContent]] = MBox2(portId"0", None())
          |
          |    ${sts}
          |    ${touchTypes(typeTouches)}
          |
          |    ${(apiTouches, "\n")}
          |  }
          |}"""
    return ret
  }

  def typeApp(packageName: String,
              instanceName: String,
              identifier: String,
              typeTouches: ISZ[ST]): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package ${packageName}.${instanceName}
          |
          |import org.sireum._
          |import art._
          |import ${packageName}._
          |
          |object ${identifier} extends App {
          |  def main(args: ISZ[String]): Z = {
          |
          |    ${touchTypes(typeTouches)}
          |
          |    return 0
          |  }
          |}
          |"""
    return ret
  }

  def app(packageName: String,
          instanceName: String,
          imports: ISZ[String],
          identifier: String,
          bridge: ST,
          bridgeIdentifier: String,
          dispatchStatus: ST,
          globals: ST,
          receiveInput: ST,
          getValue: ST,
          putValue: ST,
          sendOutput: ST,
          touchMethod: ST): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |package ${packageName}.${instanceName}
          |
          |import org.sireum._
          |import art._
          |import art.Art.BridgeId._
          |import art.Art.PortId._
          |import art.DispatchPropertyProtocol._
          |import art.PortMode._
          |${(imports, "\n")}
          |
          |object ${identifier} extends App {
          |
          |  ${bridge}
          |
          |  val entryPoints: Bridge.EntryPoints = ${bridgeIdentifier}.entryPoints
          |  val noData: Option[DataContent] = None()
          |
          |  ${globals}
          |
          |  ${dispatchStatus}
          |
          |  ${getValue}
          |
          |  ${receiveInput}
          |
          |  ${putValue}
          |
          |  ${sendOutput}
          |
          |  def initialiseArchitecture(): Unit = {
          |    // nothing to do - CAmkES is responsible for initialization
          |  }
          |
          |  def initialiseEntryPoint(): Unit = { entryPoints.initialise() }
          |
          |  def computeEntryPoint(): Unit = { entryPoints.compute() }
          |
          |  def finaliseEntryPoint(): Unit = { entryPoints.finalise() }
          |
          |  def main(args: ISZ[String]): Z = {
          |
          |    // need to touch the following for transpiler
          |    initialiseArchitecture()
          |    initialiseEntryPoint()
          |    computeEntryPoint()
          |    finaliseEntryPoint()
          |
          |    touch()
          |
          |    return 0
          |  }
          |
          |  ${touchMethod}
          |
          |  def logInfo(title: String, msg: String): Unit = {
          |    print(${bridgeIdentifier}.name)
          |    print(": ")
          |    println(msg)
          |  }
          |
          |  def logError(title: String, msg: String): Unit = {
          |    eprint(${bridgeIdentifier}.name)
          |    eprint(": ")
          |    eprintln(msg)
          |  }
          |
          |  def logDebug(title: String, msg: String): Unit = {
          |    print(${bridgeIdentifier}.name)
          |    print(": ")
          |    println(msg)
          |  }
          |
          |  def run(): Unit = {}
          |
          |}
          |"""
    return ret
  }

  def methodSignature(methodName: String, params: ISZ[ST], returnType: String): ST = {
    val preParams: ST = if (params.isEmpty) StackFrameTemplate.STACK_FRAME_ONLY_ST else StackFrameTemplate.STACK_FRAME_ST
    val ret: ST = if (params.isEmpty) {
      st"${returnType} ${methodName}(${preParams})"
    } else {
      st"""${returnType} ${methodName}(
          |  ${preParams}
          |  ${(params, ",\n")})"""
    }

    return ret
  }

  def initialize_apis(names: NameProvider,
                      filename: String): (ISZ[ST], ST) = {
    val initApiType = names.cInitializationApi
    val initApiId = names.cInitializationApi_Id

    val initApi = s"${names.packageName}.${names.apiInitialization}"
    val (initOptionSig, initSomeSig, initNoneSig) = TypeUtil.getOptionTypeFingerprints(initApi)

    val initApiSt: ST =
      st"""// ${initOptionSig} = Option[${initApi}]
          |${initOptionSig}_get_(${StackFrameTemplate.SF} (${initApiType}) &initialization_api, ${initApiId}(${StackFrameTemplate.SF_LAST}));"""

    val operApiType = names.cOperationalApi
    val operApiId = names.cOperationalApi_Id

    val operApi = s"${names.packageName}.${names.apiOperational}"
    val (operOptionSig, operSomeSig, operNoneSig) = TypeUtil.getOptionTypeFingerprints(operApi)

    val operApiSt: ST =
      st"""// ${operOptionSig} = Option[${operApi}]
          |${operOptionSig}_get_(${StackFrameTemplate.SF} (${operApiType}) &operational_api, ${operApiId}(${StackFrameTemplate.SF_LAST}));"""

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(T, filename, "", "initialize_apis", 0)

    val method: ST =
      st"""static void initialize_apis(${StackFrameTemplate.STACK_FRAME_ONLY_ST}) {
          |  ${declNewStackFrame};
          |
          |  ${initApiSt}
          |  ${operApiSt}
          |  apis_initialized = true;
          |}"""

    val variables: ISZ[ST] = ISZ(
      st"static bool apis_initialized = false",
      st"static struct ${initApiType} initialization_api",
      st"static struct ${operApiType} operational_api")

    return (variables, method)
  }

  def apiGet(signature: ST,
             declNewStackFrame: ST,
             apiGetMethodName: String,
             typ: TypeNameProvider): ST = {

    // this CANNOT use type aliases
    val qualifiedNameForFingerprinting: String =
      typ.kind match {
        case TypeKind.Base => TypeResolver.getSlangType(typ.typeName).string
        case TypeKind.Bit => TypeUtil.BIT_SIG
        case TypeKind.Empty => typ.qualifiedReferencedTypeName
        case _ => s"${typ.basePackageName}.${typ.qualifiedReferencedTypeName}"
      }

    val (optionSig, someSig, noneSig) = TypeUtil.getOptionTypeFingerprints(qualifiedNameForFingerprinting)

    val typeAssign: Option[String] =
      if (typ.isEmptyType) {
        None[String]()
      }
      else if (typ.isBaseType || typ.isEnum) {
        Some(s"*value = t_0.${someSig}.value;")
      }
      else {
        val struct: String = if (!typ.isEnum && !typ.isBaseType) s"struct " else ""
        Some(s"Type_assign(value, &t_0.${someSig}.value, sizeof(${struct}${typ.qualifiedCTypeName}));")
      }

    val ret: ST =
      st"""${signature}{
          |  ${declNewStackFrame};
          |
          |  if(!apis_initialized) { initialize_apis(${StackFrameTemplate.SF_LAST_ST}); }
          |
          |  // ${optionSig} = Option[${qualifiedNameForFingerprinting}]
          |  // ${someSig} = Some[${qualifiedNameForFingerprinting}]
          |  DeclNew${optionSig}(t_0);
          |  ${apiGetMethodName}(
          |    SF
          |    (${optionSig}) &t_0,
          |    &operational_api);
          |
          |  if(t_0.type == T${someSig}){
          |    ${typeAssign}
          |    return true;
          |  } else {
          |    return false;
          |  }
          |}"""
    return ret
  }

  def apiGet_byteArrayVersion(signature: ST,
                              declNewStackFrame: ST,
                              apiGetMethodName: String,
                              typ: TypeNameProvider): ST = {

    // this CANNOT use type aliases
    val qualifiedNameForFingerprinting: String =
      typ.kind match {
        case TypeKind.Base => TypeResolver.getSlangType(typ.typeName).string
        case TypeKind.Bit => TypeUtil.BIT_SIG
        case TypeKind.Empty => typ.qualifiedReferencedTypeName
        case _ => s"${typ.basePackageName}.${typ.qualifiedReferencedTypeName}"
      }

    val (optionSig, someSig, noneSig) = TypeUtil.getOptionTypeFingerprints(qualifiedNameForFingerprinting)

    val ret: ST =
      st"""${signature}{
          |  ${declNewStackFrame};
          |
          |  if(!apis_initialized) { initialize_apis(${StackFrameTemplate.SF_LAST_ST}); }
          |
          |  // ${optionSig} = Option[${qualifiedNameForFingerprinting}]
          |  // ${someSig} = Some[${qualifiedNameForFingerprinting}]
          |  DeclNew${optionSig}(t_0);
          |
          |  ${apiGetMethodName}(
          |    ${StackFrameTemplate.SF}
          |    (${optionSig}) &t_0,
          |    &operational_api);
          |
          |  if(t_0.type == T${someSig}){
          |    *numBits = t_0.Some_8D03B1.value.size;
          |    if(*numBits > 0) {
          |      size_t numBytes = (*numBits - 1) / 8 + 1;
          |      memcpy(byteArray, &t_0.Some_8D03B1.value.value, numBytes);
          |    }
          |    return true;
          |  } else {
          |    return false;
          |  }
          |}"""
    return ret
  }

  def apiSet(signature: ST, declNewStackFrame: ST, apiSetMethodName: String, isEventPort: B): ST = {
    var args: ISZ[ST] = ISZ(st"&initialization_api")
    if (!isEventPort) {
      args = args :+ st"value"
    }

    val ret: ST =
      st"""${signature} {
          |  ${declNewStackFrame};
          |
          |  if(!apis_initialized) { initialize_apis(${StackFrameTemplate.SF_LAST_ST}); }
          |
          |  ${apiSetMethodName}(
          |    ${StackFrameTemplate.SF}
          |    ${(args, ",\n")});
          |}"""
    return ret
  }

  def apiSet_byteArrayVersion(signature: ST,
                              declStackFrame: ST,
                              apiSetMethodName: String): ST = {

    val args: ISZ[ST] = ISZ(
      st"&initialization_api",
      st"&t_0"
    )

    val ret: ST =
      st"""${signature} {
          |  ${declStackFrame};
          |
          |  ${StackFrameTemplate.sfAssert("(Z) numBits >= 0", "numBits must be non-negative for IS[Z, B].")}
          |  ${StackFrameTemplate.sfAssert("(Z) numBits <= MaxIS_C4F575", "numBits too large for IS[Z, B].")}
          |
          |  if(!apis_initialized) { initialize_apis(${StackFrameTemplate.SF_LAST_ST}); }
          |
          |  DeclNewIS_C4F575(t_0);
          |
          |  t_0.size = numBits;
          |  if(numBits > 0) {
          |    size_t numBytes = (numBits - 1) / 8 + 1;
          |    memcpy(&t_0.value, byteArray, numBytes);
          |  }
          |
          |  ${apiSetMethodName}(
          |    ${StackFrameTemplate.SF}
          |    ${(args, ",\n")});
          |}"""
    return ret
  }

  def apiLog(signature: ST, declNewStackFrame: ST, apiLogMethodName: String): ST = {
    val args: ISZ[ST] = ISZ(st"&initialization_api", st"str")

    val ret: ST =
      st"""${signature} {
          |  ${declNewStackFrame};
          |
          |  if(!apis_initialized) { initialize_apis(${StackFrameTemplate.SF_LAST_ST}); }
          |
          |  ${apiLogMethodName}(
          |    ${StackFrameTemplate.SF}
          |    ${(args, ",\n")});
          |}"""
    return ret
  }

  def cHeaderFile(macroName: String,
                  headerMethods: ISZ[ST]): ST = {
    val ret: ST =
      st"""#ifndef ${macroName}
          |#define ${macroName}
          |
          |#include <all.h>
          |
          |${(headerMethods, "\n\n")}
          |
          |#endif
          |"""
    return ret
  }

  def cImplFile(fileName: String,
                includes: ISZ[String],
                globalVars: ISZ[ST],
                implMethods: ISZ[ST]): ST = {
    val _includes: Option[ST] =
      if (includes.isEmpty) None()
      else Some(st"${(ops.ISZOps(includes).map((s: String) => s"#include ${s}"), "\n")}")

    val _globals: Option[ST] =
      if (globalVars.isEmpty) None()
      else Some(
        st"""${(globalVars.map((s: ST) => st"${s};"), "\n")}
            |""")

    val ret: ST =
      st"""#include <${fileName}.h>
          |${_includes}
          |
          |${_globals}
          |${(implMethods, "\n\n")}
          |"""
    return ret
  }

  def ifEsleHelper(options: ISZ[(ST, ST)], optElse: Option[ST]): ST = {
    val first: Option[(ST, ST)] = if (options.size > 0) {
      Some(options(0))
    } else {
      None()
    }
    val rest: ISZ[(ST, ST)] = if (options.size > 1) {
      org.sireum.ops.ISZOps(options).drop(1)
    } else {
      ISZ()
    }
    return ifElseST(first, rest, optElse)
  }

  def ifElseST(ifbranch: Option[(ST, ST)], elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {

    var body = st""

    if (ifbranch.nonEmpty) {
      body =
        st"""if(${ifbranch.get._1}) {
            |  ${ifbranch.get._2}
            |} """
    }

    if (elsifs.nonEmpty) {
      val ei = elsifs.map((x: (ST, ST)) =>
        st"""else if(${x._1}) {
            |  ${x._2}
            |} """)
      body = st"""${body}${ei}"""
    }

    if (els.nonEmpty) {
      if (ifbranch.nonEmpty) {
        body =
          st"""${body}else {
              |  ${els.get}
              |}"""
      } else {
        body = els.get
      }
    }

    return body
  }

  def transpilerToucher(basePackage: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package ${basePackage}
          |
          |import org.sireum._
          |
          |${CommentTemplate.safeToEditComment_scala}
          |
          |object ${TRANSPILER_TOUCHER_OBJECT_NAME} {
          |  def ${TRANSPILER_TOUCHER_METHOD_NAME}(): Unit = {
          |  }
          |}
          |"""
    return ret
  }

  def callTranspilerToucher(): ST = {
    return st"${TRANSPILER_TOUCHER_OBJECT_NAME}.${TRANSPILER_TOUCHER_METHOD_NAME}()"
  }

  def ext_h(blocks: ISZ[ST]): ST = {
    val ret: ST =
      st"""#ifndef EXT_H
          |#define EXT_H
          |
          |${CommentTemplate.safeToEditComment_c}
          |
          |#include <all.h>
          |
          |${(blocks, "\n\n")}
          |#endif"""
    return ret
  }

  def ext_c(blocks: ISZ[ST]): ST = {
    val ret: ST =
      st"""#include <ext.h>
          |
          |${CommentTemplate.safeToEditComment_c}
          |
          |// add c extension code here
          |
          |${(blocks, "\n\n")}"""
    return ret
  }

  def bitCodecExtHEnties(): ST = {
    val ret: ST =
      st"""void byte_array_default(STACK_FRAME uint8_t* byteArray, size_t numBits, size_t numBytes);
          |
          |void byte_array_string(STACK_FRAME String str, uint8_t* byteArray, size_t numBytes);
          |
          |void hex_dump(STACK_FRAME uint8_t* byte_array, size_t numBytes);"""
    return ret
  }

  def bitCodecExtCEnties(): ST = {
    val ret: ST =
      st"""// example method that sets the first numBits bits of byteArray to 1
          |void byte_array_default(STACK_FRAME uint8_t* byteArray, size_t numBits, size_t numBytes) {
          |  DeclNewStackFrame(caller, "ext.c", "", "byte_array_default", 0);
          |
          |  ${StackFrameTemplate.sfAssert("(numBits - 1) / 8  + 1 <= numBytes", "byte_array_default: numBytes * 8 must be at least numBits")}
          |
          |  for(size_t byte = 0; byte < numBytes; byte++) {
          |    uint8_t v = 0;
          |    for(uint8_t bit = 0; bit < 8; bit++) {
          |      if(byte * 8 + bit < numBits) {
          |        v |= 1UL << bit;
          |      }
          |    }
          |    byteArray[byte] = v;
          |  }
          |}
          |
          |// example method that places the hex value of the bytes in byteArray into str
          |void byte_array_string(STACK_FRAME String str, uint8_t* byteArray, size_t numBytes) {
          |  DeclNewStackFrame(caller, "ext.c", "", "byte_array_string", 0);
          |
          |  ${StackFrameTemplate.sfAssert("(str->size + numBytes) <= MaxString", "byte_array_string: Insufficient maximum for String characters, consider increasing the --max-string-size option")}
          |
          |  for(size_t byte = 0; byte < numBytes; byte++) {
          |    U8_string_(SF str, byteArray[byte]);
          |    String__append(SF str, string(" "));
          |  }
          |}
          |
          |// example method that directly prints the hex values of the bytes in byte_array
          |void hex_dump(STACK_FRAME uint8_t* byte_array, size_t numBytes) {
          |  DeclNewStackFrame(caller, "ext.c", "", "hex_dump", 0);
          |
          |  printf("[ ");
          |  for(size_t byte = 0; byte < numBytes; byte++) {
          |    if(byte != 0 && byte % 16 == 0) { printf("\n  "); }
          |    printf("%02X ", byte_array[byte]);
          |  }
          |  printf("]\n");
          |}"""
    return ret
  }
}
