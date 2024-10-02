// #Sireum

package org.sireum.hamr.arsit.nix

import org.sireum._
import org.sireum.hamr.arsit.Util.nameProvider
import org.sireum.hamr.arsit._
import org.sireum.hamr.arsit.templates.SeL4NixTemplate
import org.sireum.hamr.arsit.util.ReporterUtil.reporter
import org.sireum.hamr.arsit.util.{ArsitOptions, ArsitPlatform}
import org.sireum.hamr.codegen.common._
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.templates.{CommentTemplate, StackFrameTemplate}
import org.sireum.hamr.codegen.common.templates.StackFrameTemplate.{SF, SF_LAST}
import org.sireum.hamr.codegen.common.types._
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir

object NixGen {
  val IPC_C: String = "ipc.c"
  val EXT_H: String = "ext.h"
  val EXT_C: String = "ext.c"

  val KNOWN_HAMR_PROVIDED_FILES: ISZ[String] = ISZ(IPC_C, EXT_H, EXT_C)

  def willTranspile(platform: ArsitPlatform.Type): B = {
    val ret: B = platform match {
      case ArsitPlatform.Linux => T
      case ArsitPlatform.MacOS => T
      case ArsitPlatform.Cygwin => T
      case ArsitPlatform.SeL4 => T
      case _ => F
    }
    return ret
  }

  def genTypeTouches(types: AadlTypes): ISZ[ST] = {
    var a: ISZ[ST] = ISZ()

    var seen: Set[String] = Set.empty
    for (typ <- types.typeMap.values if !seen.contains(typ.name)) {
      seen = seen + typ.name
      a = a :+ SeL4NixTemplate.touchType(typ.nameProvider.qualifiedPayloadName, Some(typ.nameProvider.example()))
    }
    a = a :+ SeL4NixTemplate.touchType("art.Empty", None())
    return a
  }

  def genApiTouches(types: AadlTypes, basePackage: String, threadOrDevices: ISZ[AadlThreadOrDevice]): ISZ[ST] = {
    val sts: ISZ[ST] = threadOrDevices.map(threadOrDevice => {
      val component = threadOrDevice.component

      val names = nameProvider(component, basePackage)
      val ports: ISZ[Port] = Util.getPorts(threadOrDevice, types, basePackage, z"0")

      st"""{
          |  ${(SeL4NixTemplate.apiTouches(names, ports), "\n")}
          |}"""
    })
    return sts
  }
}

@msig trait NixGen {
  def dirs: ProjectDirectories

  def root: AadlSystem

  def arsitOptions: ArsitOptions

  def symbolTable: SymbolTable

  def types: AadlTypes

  def previousPhase: Result

  def generate(): ArsitResult

  def genExtensionEntries(basePackage: String, components: ISZ[AadlThreadOrDevice]): (ISZ[ST], ISZ[ST]) = {
    var extHEntries: ISZ[ST] = ISZ()
    var extCEntries: ISZ[ST] = ISZ()

    if (types.rawConnections) {
      // add numBit and numBytes global vars for each type passing between components

      val maxBitSize: Z = TypeUtil.getMaxBitsSize(symbolTable) match {
        case Some((z, _)) => z
        case _ =>
          // model must only contain event ports (i.e not data ports)
          1
      }

      var seenTypes: Set[AadlType] = Set.empty

      for (threadOrDevice <- components) {
        val names = nameProvider(threadOrDevice.component, basePackage)
        val ports: ISZ[Port] = Util.getPorts(threadOrDevice, types, basePackage, z"0")

        for (p <- ports.filter(p => CommonUtil.isDataPort(p.feature))) {
          val originatingType: AadlType = p._portType match {
            case BitType(_, _, _, _, Some(o)) => o
            case _ => halt(s"Unexpected: Could not find originating type for ${p._portType} used by ${p.parentName}.${p.path}")
          }
          if (!seenTypes.contains(originatingType)) {
            seenTypes = seenTypes + originatingType

            val (bitSize, optionalMessage): (Z, Option[String]) = originatingType.bitSize match {
              case Some(z) => (z, None())
              case _ =>
                if (symbolTable.isConnected(p.feature)) {
                  // symbol checking should mean this is infeasible
                  halt(s"${originatingType.name} is used by connected port ${p.parentName}.${p.name} but it doesn't have a bit size specified")
                }
                val msg = s"${originatingType.name} does not specify a bit size, assuming max bit size of ${maxBitSize}. Used by unconnected port ${p.parentName}.${p.name}"
                reporter.warn(None(), Util.toolName, msg)

                (maxBitSize, Some(s"// ${msg}"))
            }

            val numBitsName = BitCodecNameUtil.numBitsConstName(originatingType.nameProvider.qualifiedCTypeName)
            val numBytesName = BitCodecNameUtil.numBytesConstName(originatingType.nameProvider.qualifiedCTypeName)

            extHEntries = extHEntries :+
              st"""// bit-codec size for ${originatingType.nameProvider.qualifiedCTypeName}
                  |${optionalMessage}
                  |#define ${numBitsName} ${bitSize}
                  |#define ${numBytesName} ((${numBitsName} - 1) / 8 + 1)"""

          }
        }
      }
      extHEntries = extHEntries :+ SeL4NixTemplate.bitCodecExtHEnties()
      extCEntries = extCEntries :+ SeL4NixTemplate.bitCodecExtCEnties()
    }
    return (extHEntries, extCEntries)
  }

  def genExtensionFiles(threadOrDevice: AadlThreadOrDevice, names: NameProvider, ports: ISZ[Port]): (ISZ[Os.Path], ISZ[FileResource]) = {

    val rootExtDir = Os.path(dirs.cExt_c_Dir)

    var extensionFiles: ISZ[Os.Path] = ISZ()
    var resources: ISZ[FileResource] = ISZ()

    if (arsitOptions.excludeImpl || symbolTable.hasCakeMLComponents()) {

      val componentName = names.cComponentType
      val extRoot = rootExtDir / names.componentSingletonType

      val userImplFile = extRoot / s"${names.componentSingletonType}.c"
      val userHeaderFile = extRoot / s"${names.componentSingletonType}.h"

      val apiFilename = NixSeL4NameUtil.apiHelperFilename(names)
      val apiHeaderFile = extRoot / s"${apiFilename}.h"
      val apiImplFile = extRoot / s"${apiFilename}.c"

      var entrypointAdapters: ISZ[ST] = ISZ()

      val logInfo = NixSeL4NameUtil.apiHelperMethodName("logInfo", names)

      if (arsitOptions.excludeImpl) { // add entrypoint stubs
        var entrypointSignatures: ISZ[ST] = ISZ()

        val params: ISZ[ST] = ISZ()

        val (initMethodSig, initMethodImpl, initAdapterMethod) = genStubInitializeMethod(names, ports, apiImplFile.name, userImplFile.name)
        val (finalizeMethodSig, finalizeMethodImpl, finalizeAdapterMethod) = genStubFinaliseMethod(names, apiImplFile.name, userImplFile.name)

        entrypointAdapters = entrypointAdapters :+ initAdapterMethod
        entrypointAdapters = entrypointAdapters :+ finalizeAdapterMethod

        entrypointSignatures = (entrypointSignatures :+ initMethodSig) :+ finalizeMethodSig

        var methods: ISZ[ST] = ISZ(initMethodImpl, finalizeMethodImpl)

        var exampleApiUsage: ISZ[ST] = ISZ()
        var tindex = z"0"

        for (p <- ports.filter(f => CommonUtil.isInPort(f.feature))) {
          val getter = NixSeL4NameUtil.apiHelperGetterMethodName(p.name, names)
          val str = s"${p.name}_str"
          val portType: String = {
            if (CommonUtil.isAadlDataPort(p.feature)) {
              "data"
            }
            else if (CommonUtil.isAadlEventDataPort(p.feature)) {
              "event data"
            }
            else if (CommonUtil.isAadlEventPort(p.feature)) {
              "event"
            }
            else {
              halt(s"Unexpected port type: ${p}")
            }
          }

          val s: ST = if (CommonUtil.isDataPort(p.feature)) {
            val t = s"t$tindex"
            tindex = tindex + 1

            val entry: ST = {
              if (types.rawConnections) {
                val originatingTypeNames: TypeNameProvider = p._portType match {
                  case BitType(_, _, _, _, Some(o)) => o.nameProvider
                  case _ => halt(s"Unexpected: Could not find originating type for ${p._portType}")
                }

                val numBits = BitCodecNameUtil.numBitsConstName(originatingTypeNames.qualifiedCTypeName)
                val numBytes = BitCodecNameUtil.numBytesConstName(originatingTypeNames.qualifiedCTypeName)
                val bitsName = s"${t}_numBits"

                st"""uint8_t ${t}[${numBytes}];
                    |size_t ${bitsName};
                    |if(${getter}(${SF} &${bitsName}, ${t})) {
                    |  // sanity check
                    |  ${StackFrameTemplate.sfAssert(s"(Z) ${bitsName} == ${numBits}", "numBits received does not match expected")}
                    |
                    |  printf("%s: Received data on ${portType} port ${p.name}: \n", component_id);
                    |  hex_dump(${SF} ${t}, ${numBytes});
                    |
                    |  /* alternative using logInfo.  Commented out as the constructed String may be too large
                    |  DeclNewString(${str});
                    |  String__append(${SF} (String) &${str}, string("Received data on ${portType} port ${p.name}: "));
                    |  byte_array_string(${SF} (String) &${str}, ${t}, ${numBytes});
                    |  ${logInfo}(${SF} (String) &${str});
                    |  */
                    |}"""
              }
              else {
                val (refName, decl): (String, ST) = if (p.getPortTypeNames.isEnum || p.getPortTypeNames.isBaseType) {
                  (t, st"${p.getPortTypeNames.qualifiedCTypeName} $t;")
                } else {
                  (s"&$t", st"DeclNew${p.getPortTypeNames.qualifiedCTypeName}($t);")
                }
                st"""${decl}
                    |if(${getter}(${SF} &${t})) {
                    |  printf("%s: Received data on ${portType} port ${p.name}: \n", component_id);
                    |
                    |  /* alternative using logInfo.  Commented out as the constructed String may be too large
                    |  DeclNewString(${str});
                    |  String__append(${SF} (String) &${str}, string("Received data on ${portType} port ${p.name}: "));
                    |  ${p.getPortTypeNames.qualifiedCTypeName}_string_(${SF} (String) &${str}, ${refName});
                    |  ${logInfo}(${SF} (String) &${str});
                    |  */
                    |}"""
              }
            }
            entry
          } else {
            st"""if(${getter}(${SF_LAST} )){
                |  printf("%s: Received event on ${p.name}\n", component_id);
                |
                |  /* alternative using logInfo.  Commented out as the constructed String may be too large
                |  String ${str} = string("Received event on ${portType} port ${p.name}");
                |  ${logInfo}(${SF} ${str});
                |  */
                |}"""
          }
          exampleApiUsage = exampleApiUsage :+ s
        }

        val _exampleApiUsage: Option[ST] =
          if (exampleApiUsage.isEmpty) None()
          else Some(
            st"""// examples of api getter usage
                |
                |${(exampleApiUsage, "\n\n")}""")

        threadOrDevice.dispatchProtocol match {
          case Dispatch_Protocol.Periodic =>
            // timetriggered
            val apiMethodName = s"${componentName}_timeTriggered"
            val userMethodName = s"${apiMethodName}_"
            val timeTriggeredSig = SeL4NixTemplate.methodSignature(userMethodName, params, "Unit")
            entrypointSignatures = entrypointSignatures :+ timeTriggeredSig
            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = userImplFile.name,
              owner = "",
              name = userMethodName,
              line = 0)

            methods = methods :+
              st"""${timeTriggeredSig} {
                  |  ${declNewStackFrame};
                  |
                  |  ${_exampleApiUsage}
                  |}"""

            val api_params = ISZ(st"${names.cOperationalApi} api")
            val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
            val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = apiMethodName,
              line = 0)
            val apiAdapterMethod: ST =
              st"""${apiMethodSig} {
                  |  ${apiDeclNewStackFrame};
                  |
                  |  ${userMethodName}(${SF_LAST});
                  |}"""

            entrypointAdapters = entrypointAdapters :+ apiAdapterMethod

          case Dispatch_Protocol.Sporadic =>
            val inEventPorts = ports.filter(f => CommonUtil.isEventPort(f.feature) && CommonUtil.isInFeature(f.feature))

            var dumpedExampleGetterApiUsageAlready: B = F
            for (p <- inEventPorts) {
              val portType: String = {
                if (CommonUtil.isAadlDataPort(p.feature)) {
                  "data"
                }
                else if (CommonUtil.isAadlEventDataPort(p.feature)) {
                  "event data"
                }
                else if (CommonUtil.isAadlEventPort(p.feature)) {
                  "event"
                }
                else {
                  halt(s"Unexpected port type: ${p}")
                }
              }
              val isEventData: B = CommonUtil.isAadlEventDataPort(p.feature)
              val handlerName = s"${componentName}_handle_${p.name}"
              val apiMethodName = handlerName
              val handlerMethodName = s"${handlerName}_"

              var eventDataParams: ISZ[ST] = params
              if (isEventData) {
                eventDataParams = eventDataParams :+ st"${p._portType.nameProvider.qualifiedCTypeName} value"
              }
              val handlerSig = SeL4NixTemplate.methodSignature(handlerMethodName, eventDataParams, "Unit")
              entrypointSignatures = entrypointSignatures :+ handlerSig

              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = userImplFile.name,
                owner = "",
                name = handlerMethodName,
                line = 0)

              if (types.rawConnections && isEventData) {
                val rawHandlerMethodName = s"${handlerName}_raw"
                val numBits = "numBits"
                val byteArray = "byteArray"
                val rawParams: ISZ[ST] = params :+ st"size_t ${numBits}" :+ st"uint8_t *${byteArray}"

                val rawHandler = SeL4NixTemplate.methodSignature(rawHandlerMethodName, rawParams, "Unit")

                val declNewStackFrameRaw: ST = StackFrameTemplate.DeclNewStackFrame(
                  caller = T,
                  uri = apiImplFile.name,
                  owner = "",
                  name = rawHandlerMethodName,
                  line = 0)

                val str = s"${p.name}String"
                methods = methods :+
                  st"""${rawHandler} {
                      |  ${declNewStackFrameRaw};
                      |
                      |  size_t numBytes = ${numBits} == 0 ? 0 : (${numBits} - 1) / 8 + 1;
                      |
                      |  printf("%s: ${rawHandlerMethodName} called with payload: \n", component_id);
                      |  hex_dump(${SF} ${byteArray}, numBytes);
                      |
                      |  /* alternative using logInfo.  Commented out as the constructed String may be too large
                      |  DeclNewString(${p.name}String);
                      |  String__append(${SF} (String) &${str}, string("${rawHandlerMethodName} called with payload: "));
                      |  byte_array_string(${SF} (String) &${str}, ${byteArray}, numBytes);
                      |  ${logInfo} (${SF} (String) &${str});
                      |  */
                      |}"""

                val __exampleApiUsage: Option[ST] =
                  if (!dumpedExampleGetterApiUsageAlready && _exampleApiUsage.nonEmpty) {
                    dumpedExampleGetterApiUsageAlready = T
                    _exampleApiUsage
                  } else {
                    None()
                  }

                methods = methods :+
                  st"""${handlerSig} {
                      |  ${declNewStackFrame};
                      |
                      |  ${rawHandlerMethodName}(${SF} value->size, value->value);
                      |
                      |  ${__exampleApiUsage}
                      |}"""
              }
              else {
                val printValue: ST = if (isEventData) {
                  st"""printf("%s: Received data on ${portType} port ${p.name}: \n", component_id);
                      |
                      |/* alternative using logInfo.  Commented out as the constructed String may be too large
                      |DeclNewString(_str);
                      |String__append(${SF} (String) &_str, string("Received on ${p.name}: "));
                      |${p.getPortTypeNames.qualifiedCTypeName}_string_(${SF} (String) &_str, value);
                      |${logInfo}(${SF} (String) &_str);
                      |*/"""
                } else {
                  st"""printf("%s: Received event on ${portType} port ${p.name}: \n", component_id);
                      |
                      |/* alternative using logInfo.  Commented out as the constructed String may be too large
                      |String str = string("Received event on ${p.name}");
                      |${logInfo}(${SF} str);
                      |*/"""
                }

                val __exampleApiUsage: Option[ST] =
                  if (!dumpedExampleGetterApiUsageAlready && _exampleApiUsage.nonEmpty) {
                    dumpedExampleGetterApiUsageAlready = T
                    _exampleApiUsage
                  } else {
                    None()
                  }

                methods = methods :+
                  st"""${handlerSig} {
                      |  ${declNewStackFrame};
                      |
                      |  printf("%s: ${handlerName} called\n", component_id);
                      |
                      |  /* alternative using logInfo.  Commented out as the constructed String may be too large
                      |  DeclNewString(${p.name}String);
                      |  String__append(${SF} (String) &${p.name}String, string("${handlerName} called"));
                      |  ${logInfo} (${SF} (String) &${p.name}String);
                      |  */
                      |
                      |  ${printValue}
                      |
                      |  ${__exampleApiUsage}
                      |}"""
              }

              val api_params = ISZ(st"${names.cOperationalApi} api") ++ eventDataParams
              val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
              val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = apiMethodName,
                line = 0)

              val valueArg: String = if (isEventData) s"${SF} value" else SF_LAST

              val apiAdapterMethod: ST =
                st"""${apiMethodSig} {
                    |  ${apiDeclNewStackFrame};
                    |
                    |  ${handlerMethodName}(${valueArg});
                    |}"""

              entrypointAdapters = entrypointAdapters :+ apiAdapterMethod
            }
          case x => halt(s"Unexpected dispatch protocol ${x}")
        }

        {
          extensionFiles = extensionFiles :+ userHeaderFile
          val userHeaderMethods = ops.ISZOps(entrypointSignatures).map((s: ST) => st"${s};")
          val userMacroName = StringUtil.toUpperCase(s"${names.componentSingletonType}_h")
          val headerSt = SeL4NixTemplate.cHeaderFile(userMacroName, userHeaderMethods)
          resources = resources :+ ResourceUtil.createResource(
            Util.pathAppend(userHeaderFile.up.value, ISZ(userHeaderFile.name)), headerSt, T)
        }

        val impl =
          st"""#include <${apiHeaderFile.name}>
              |#include <${userHeaderFile.name}>
              |#include <${NixGen.EXT_H}>
              |
              |${CommentTemplate.safeToEditComment_c}
              |
              |static char* component_id = "${names.instanceName}";
              |
              |${(methods, "\n\n")}
              |"""

        {
          val implFile = extRoot / s"${names.componentSingletonType}.c"
          extensionFiles = extensionFiles :+ implFile
          resources = resources :+ ResourceUtil.createResource(
            Util.pathAppend(implFile.up.value, ISZ(implFile.name)), impl, F)
        }
      }

      // api helper methods (cakeml ffi's link against the c helper apis)
      if (arsitOptions.excludeImpl || threadOrDevice.isCakeMLComponent()) {

        var headerMethods: ISZ[ST] = ISZ(st"${CommentTemplate.doNotEditComment_c}")
        var implMethods: ISZ[ST] = ISZ(st"${CommentTemplate.doNotEditComment_c}")

        for (p <- ports) {
          val typeNames = p._portType.nameProvider

          p.feature.direction match {
            case ir.Direction.In => {

              val cApiMethodName = NixSeL4NameUtil.apiHelperGetterMethodName(p.name, names)
              val returnType = "bool"
              val slangApiGetMethodName = s"${names.cOperationalApi}_get_${p.name}_"
              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = cApiMethodName,
                line = 0)

              if (types.rawConnections && CommonUtil.isDataPort(p.feature)) {
                // provide alt byte array version

                val altParams = ISZ(
                  st"size_t *numBits",
                  st"uint8_t *byteArray")

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, altParams, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet_byteArrayVersion(
                  signature = altSignature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  typ = typeNames)
              } else {
                val pointer: String = if (typeNames.isEnum || typeNames.isBaseType) "*" else ""

                var params: ISZ[ST] = ISZ()
                if (!typeNames.isEmptyType) {
                  params = params :+ st"${typeNames.qualifiedCTypeName} ${pointer}value"
                }

                val signature = SeL4NixTemplate.methodSignature(cApiMethodName, params, returnType)

                headerMethods = headerMethods :+ st"${signature};"

                implMethods = implMethods :+ SeL4NixTemplate.apiGet(
                  signature = signature,
                  declNewStackFrame = declNewStackFrame,
                  apiGetMethodName = slangApiGetMethodName,
                  typ = typeNames)
              }
            }
            case ir.Direction.Out => {
              val isEventPort = p.feature.category == ir.FeatureCategory.EventPort

              val cApiMethodName = NixSeL4NameUtil.apiHelperSetterMethodName(p.name, names)
              val returnType = "void"
              val slangApiSetMethodName = s"${names.cInitializationApi}_put_${p.name}_"
              val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
                caller = T,
                uri = apiImplFile.name,
                owner = "",
                name = cApiMethodName,
                line = 0)

              if (types.rawConnections && CommonUtil.isDataPort(p.feature)) {
                // provide alt byte array version

                val altParams = ISZ(
                  st"size_t numBits",
                  st"uint8_t *byteArray"
                )
                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, altParams, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet_byteArrayVersion(
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName)
              } else {

                var params: ISZ[ST] = ISZ()
                if (!isEventPort) {
                  params = params :+ st"${typeNames.qualifiedCTypeName} value"
                }

                val altSignature = SeL4NixTemplate.methodSignature(cApiMethodName, params, returnType)

                headerMethods = headerMethods :+ st"${altSignature};"
                implMethods = implMethods :+ SeL4NixTemplate.apiSet(
                  altSignature,
                  declNewStackFrame,
                  slangApiSetMethodName,
                  isEventPort)
              }
            }
            case x => halt(s"Unexpected direction ${x}")
          }
        }

        { // logging methods

          val loggers = ISZ("logInfo", "logDebug", "logError")

          for (l <- loggers) {
            val methodName = NixSeL4NameUtil.apiHelperMethodName(l, names)
            val params = ISZ(st"String str")

            val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
              caller = T,
              uri = apiImplFile.name,
              owner = "",
              name = methodName,
              line = 0)

            val signature = SeL4NixTemplate.methodSignature(methodName, params, "void")

            val apiLogMethodName = s"${names.cInitializationApi}_${l}_"

            headerMethods = headerMethods :+ st"${signature};"
            implMethods = implMethods :+ SeL4NixTemplate.apiLog(signature, declNewStackFrame, apiLogMethodName)
          }
        }

        // entrypointAdapters will be empty if not excluding slang component impls
        implMethods = implMethods ++ entrypointAdapters

        val macroName = StringUtil.toUpperCase(s"${apiFilename}_h")

        val headerContents = SeL4NixTemplate.cHeaderFile(macroName, headerMethods)

        val (apiGlobalVars, apiInitMethod): (ISZ[ST], ST) = SeL4NixTemplate.initialize_apis(names, userImplFile.name)
        implMethods = apiInitMethod +: implMethods

        val includes: ISZ[String] = if (arsitOptions.excludeImpl) ISZ(s"<${userHeaderFile.name}>") else ISZ()

        val implContents = SeL4NixTemplate.cImplFile(apiFilename, includes, apiGlobalVars, implMethods)

        extensionFiles = extensionFiles :+ apiHeaderFile
        extensionFiles = extensionFiles :+ apiImplFile

        resources = resources :+ ResourceUtil.createResource(Util.pathAppend(apiHeaderFile.up.value, ISZ(apiHeaderFile.name)), headerContents, T)
        resources = resources :+ ResourceUtil.createResource(Util.pathAppend(apiImplFile.up.value, ISZ(apiImplFile.name)), implContents, T)
      } // end helper api methods
    }

    return (extensionFiles, resources)
  }

  def genStubInitializeMethod(names: NameProvider, ports: ISZ[Port], apiFileUri: String, userFileUri: String): (ST, ST, ST) = {
    val params: ISZ[ST] = ISZ()
    val apiMethodName = s"${names.cComponentType}_initialise"
    val userMethodName = s"${apiMethodName}_"
    val initialiseMethodSig = SeL4NixTemplate.methodSignature(userMethodName, params, "Unit")

    var statements: ISZ[ST] = ISZ()

    var resultCount = z"0"
    for (p <- ports.filter(f => CommonUtil.isOutPort(f.feature))) {
      val setterName = NixSeL4NameUtil.apiHelperSetterMethodName(p.name, names)
      val u: ST = if (CommonUtil.isDataPort(p.feature)) {
        val result = s"t${resultCount}"
        resultCount = resultCount + 1
        val decl: ST =
          if (types.rawConnections) {
            val originatingTypeNames: TypeNameProvider = p._portType match {
              case BitType(_, _, _, _, Some(o)) => o.nameProvider
              case _ => halt(s"Unexpected: Could not find originating type for ${p._portType}")
            }

            val numBits = BitCodecNameUtil.numBitsConstName(originatingTypeNames.qualifiedCTypeName)
            val numBytes = BitCodecNameUtil.numBytesConstName(originatingTypeNames.qualifiedCTypeName)

            st"""uint8_t ${result}[${numBytes}];
                |byte_array_default(${SF} ${result}, ${numBits}, ${numBytes});
                |${setterName}(${SF} ${numBits}, ${result});"""

          } else {
            if (p.getPortTypeNames.isEnum) {
              st"""${p.getPortTypeNames.qualifiedCTypeName} ${result} = ${p.getPortTypeNames.example_C_Name()};
                  |${setterName}(${SF} ${result});"""
            } else if (p.getPortTypeNames.isBaseType) {
              st"""${p.getPortTypeNames.qualifiedCTypeName} ${result} = ${p.getPortTypeNames.example_C_Name()}(${SF_LAST});
                  |${setterName}(${SF} ${result});"""
            } else {
              st"""DeclNew${p.getPortTypeNames.qualifiedCTypeName}(${result});
                  |${p.getPortTypeNames.example_C_Name()}(${SF} &${result});
                  |${setterName}(${SF} &${result});"""
            }
          }
        decl
      } else {
        st"${setterName}(${SF_LAST});"
      }

      statements = statements :+ u
    }

    val loggers: ISZ[ST] = ISZ[String]("logInfo", "logDebug", "logError").map((l: String) => {
      val mname = NixSeL4NameUtil.apiHelperMethodName(l, names)
      st"""${mname}(${SF} string("Example ${l}"));"""
    })

    //statements = statements ++ loggers

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = userFileUri,
      owner = "",
      name = userMethodName,
      line = 0)

    val initMethodImpl: ST =
      st"""${initialiseMethodSig} {
          |  ${declNewStackFrame};
          |
          |  printf("%s: ${userMethodName} called\n", component_id);
          |
          |  // example usage of api setters
          |
          |  ${(statements, "\n\n")}
          |
          |  /* example usage of api loggers. Commented out as the constructed String may be too long
          |  ${(loggers, "\n\n")}
          |  */
          |}"""

    val api_params = ISZ(st"${names.cInitializationApi} api")
    val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
    val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = apiFileUri,
      owner = "",
      name = apiMethodName,
      line = 0)
    val apiMethodImpl: ST =
      st"""${apiMethodSig} {
          |  ${apiDeclNewStackFrame};
          |
          |  ${userMethodName}(${SF_LAST});
          |}"""
    return (initialiseMethodSig, initMethodImpl, apiMethodImpl)
  }

  def genStubFinaliseMethod(names: NameProvider, apiFileUri: String, userFileUri: String): (ST, ST, ST) = {
    val params: ISZ[ST] = ISZ()
    val apiMethodName = s"${names.cComponentType}_finalise"
    val userMethodName = s"${apiMethodName}_"
    val finaliseMethodSig = SeL4NixTemplate.methodSignature(userMethodName, params, "Unit")

    val declNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = userFileUri,
      owner = "",
      name = userMethodName,
      line = 0)

    val ret: ST =
      st"""${finaliseMethodSig} {
          |  ${declNewStackFrame};
          |}"""

    val api_params = ISZ(st"${names.cOperationalApi} api")
    val apiMethodSig = SeL4NixTemplate.methodSignature(apiMethodName, api_params, "Unit")
    val apiDeclNewStackFrame: ST = StackFrameTemplate.DeclNewStackFrame(
      caller = T,
      uri = apiFileUri,
      owner = "",
      name = apiMethodName,
      line = 0)
    val adapterMethod: ST =
      st"""${apiMethodSig} {
          |  ${apiDeclNewStackFrame};
          |
          |  ${userMethodName}(${SF_LAST});
          |}"""
    return (finaliseMethodSig, ret, adapterMethod)
  }

  def genBitArraySequenceSizes(): Option[(Z, String)] = {
    if (types.rawConnections) {
      return TypeUtil.getMaxBitsSize(symbolTable)
    } else {
      return None()
    }
  }
}

object NixGenDispatch {

  def generate(dirs: ProjectDirectories,
               root: AadlSystem,
               arsitOptions: ArsitOptions,
               symbolTable: SymbolTable,
               types: AadlTypes,
               previousPhase: Result): ArsitResult = {

    val ret: ArsitResult = arsitOptions.platform match {
      case ArsitPlatform.Linux =>
        ArtNixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case ArsitPlatform.Cygwin =>
        ArtNixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case ArsitPlatform.MacOS =>
        ArtNixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case ArsitPlatform.SeL4 =>
        SeL4NixGen(dirs, root, arsitOptions, symbolTable, types, previousPhase).generate()
      case _ =>
        ArsitResult(
          resources = previousPhase.resources,
          auxResources = previousPhase.auxResources,
          maxPort = previousPhase.maxPort,
          maxComponent = previousPhase.maxComponent,
          maxConnection = previousPhase.maxConnection
        )
    }
    return ret
  }
}
