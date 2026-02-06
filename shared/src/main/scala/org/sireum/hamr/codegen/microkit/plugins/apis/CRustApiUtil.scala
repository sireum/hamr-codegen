// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.apis

import org.sireum._
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeNameProvider, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.{rust => RAST}
import org.sireum.hamr.ir.Direction

object CRustApiUtil {

  def processInPort(dstThread: AadlThread, dstPort: AadlPort,
                    crustTypeProvider: CRustTypeProvider): ComponentApiContributions = {
    val portType: AadlType = crustTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(dstPort))
    val portTypeNameProvider = crustTypeProvider.getTypeNameProvider(portType)

    val (externApiTestVariable, externApiMethod) = getCrustTestingArtifacts(dstPort, portType.classifier, portTypeNameProvider)

    return ComponentApiContributions.empty(
      externCApis = ISZ(getCExternMethodSig(dstPort, portType, portTypeNameProvider)),
      unsafeExternCApiWrappers = ISZ(getUnsafeGetWrapper(dstPort, portType, crustTypeProvider)),
      externApiTestMockVariables = ISZ(externApiTestVariable),
      externApiTestingApis = ISZ(externApiMethod),

      //putApis = ISZ(),
      unverifiedGetApis = ISZ(getBridgeGetApi(dstPort, portType, crustTypeProvider)),

      //appApiDefaultPutters = ISZ(),
      appApiDefaultGetters = ISZ(getApiDefaultGetter(dstThread, dstPort, portType, crustTypeProvider)),

      ghostVariables = ISZ(getGhostVariable(dstPort, portType, portTypeNameProvider)),
      ghostInitializations = ISZ(getGhostInitializations(dstPort, portType, crustTypeProvider))
    )
  }

  @pure def processOutPort(srcThread: AadlThread, srcPort: AadlPort, crustTypeProvider: CRustTypeProvider): ComponentApiContributions = {
    val portType: AadlType = crustTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(srcPort))
    val portTypeNameProvider = crustTypeProvider.getTypeNameProvider(portType)

    val (externApiTestVariable, externApiMethod) = getCrustTestingArtifacts(srcPort, portType.classifier, portTypeNameProvider)

    return ComponentApiContributions.empty(
      externCApis = ISZ(getCExternMethodSig(srcPort, portType, portTypeNameProvider)),
      unsafeExternCApiWrappers = ISZ(getUnsafePutWrapper(srcPort, portType, crustTypeProvider)),
      externApiTestMockVariables = ISZ(externApiTestVariable),
      externApiTestingApis = ISZ(externApiMethod),

      unverifiedPutApis = ISZ(getBridgePutApi(srcPort, portType, crustTypeProvider)),
      //getApis = ISZ(),

      appApiDefaultPutters = ISZ(getApiDefaultPutter(srcThread, srcPort, portType, crustTypeProvider)),
      //appApiDefaultGetters = ISZ(),

      ghostVariables = ISZ(getGhostVariable(srcPort, portType, portTypeNameProvider)),
      ghostInitializations = ISZ(getGhostInitializations(srcPort, portType, crustTypeProvider))
    )
  }

  @pure def getGhostInitializations(port: AadlPort, a: AadlType, crustTypeProvider: CRustTypeProvider): RAST.Item = {
    return (
      if (port.isInstanceOf[AadlDataPort])
      RAST.ItemST(st"${getGhostName(port)}: ${MicrokitTypeUtil.getCRustTypeDefaultVerusValue(a, crustTypeProvider)}")
      else RAST.ItemST(st"${getGhostName(port)}: None"))
  }

  @pure def getGhostName(port: AadlPort): String = {
    return port.identifier
  }

  @pure def getGhostVariable(port: AadlPort, aadlType: AadlType, typeNameProvider: CRustTypeNameProvider): RAST.Item = {
    val ghostName = getGhostName(port)
    val fieldType: ISZ[ISZ[String]] =
      port match {
        case i:AadlEventPort => ISZ(typeNameProvider.qualifiedRustNameS)
        case i:AadlDataPort => ISZ(typeNameProvider.qualifiedRustNameS)
        case i:AadlEventDataPort => ISZ(ISZ("Option"), typeNameProvider.qualifiedRustNameS)
      }
    return RAST.StructField(
      visibility = RAST.Visibility.Public,
      isGhost = T,
      ident = RAST.IdentString(ghostName),
      fieldType = RAST.TyPath(fieldType, Some(aadlType.classifier)))
  }

  @pure def getCExternMethodSig(port: AadlPort, aadlType: AadlType, crustTypeNameProvider: CRustTypeNameProvider): RAST.FnSig = {
    val methodName: String =
      if (port.direction == Direction.In) s"get_${port.identifier}"
      else s"put_${port.identifier}"
    val args: ISZ[RAST.Param] =
      if (port.isInstanceOf[AadlEventPort]) ISZ()
      else ISZ(
        RAST.ParamImpl(
          ident = RAST.IdentString("value"),
          kind = RAST.TyPtr(
            RAST.MutTy(
              ty = RAST.TyPath(ISZ(crustTypeNameProvider.qualifiedRustNameS), Some(aadlType.classifier)),
              mutbl = RAST.Mutability.Mut))))
    return RAST.FnSig(
      verusHeader = None(),
      fnHeader = RAST.FnHeader(F),
      ident = RAST.IdentString(methodName),
      generics = None(),
      fnDecl = RAST.FnDecl(inputs = args, outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)))
  }

  @pure def getUnsafePutWrapper(srcPort: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RAST.Fn = {
    val externMethodName = s"put_${srcPort.identifier}"
    val unsafeMethodName = s"unsafe_$externMethodName"
    val tn = crustTypeProvider.getTypeNameProvider(aadlType)
    val inputs: ISZ[RAST.Param] =
      if (srcPort.isInstanceOf[AadlEventPort]) ISZ()
      else ISZ(
        RAST.ParamImpl(
          ident = RAST.IdentString("value"),
          kind = RAST.TyRef(
            lifetime = None(),
            mutty = RAST.MutTy(
              ty = RAST.TyPath(ISZ(tn.qualifiedRustNameS), Some(aadlType.classifier)),
              mutbl = RAST.Mutability.Not))))
    val body: ST =
      st"""unsafe {
          |  return $externMethodName(value as *const ${tn.qualifiedRustName} as *mut ${tn.qualifiedRustName});
          |}"""
    return RAST.FnImpl(
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString(unsafeMethodName),
        fnDecl = RAST.FnDecl(
          inputs = inputs,
          outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)
        ),
        verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(), contract = None(), meta = ISZ(),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(body)))))
  }

  @pure def getUnsafeGetWrapper(port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RAST.Fn = {
    val tn = crustTypeProvider.getTypeNameProvider(aadlType)
    val externCMethodName = s"get_${port.identifier}"
    val unsafeMethodName = s"unsafe_$externCMethodName"
    val defaultValue = MicrokitTypeUtil.getCRustTypeDefaultValue(aadlType, crustTypeProvider)
    var returnType: RAST.Ty = MicrokitTypeUtil.rustBoolType
    val body: ST =
      port match {
        case i: AadlEventDataPort =>
          returnType = RAST.TyPath(ISZ(ISZ("Option"), tn.qualifiedRustNameS), Some(aadlType.classifier))

          st"""unsafe {
              |  let value: *mut ${tn.qualifiedRustName} = &mut $defaultValue;
              |  if ($externCMethodName(value)) {
              |    return Some(*value);
              |  } else {
              |    return None;
              |  }
              |}"""
        case i: AadlDataPort =>
          returnType = RAST.TyPath(ISZ(tn.qualifiedRustNameS), Some(aadlType.classifier))

          st"""unsafe {
              |  let value: *mut ${tn.qualifiedRustName} = &mut $defaultValue;
              |  $externCMethodName(value);
              |  return *value;
              |}"""
        case i: AadlEventPort =>
          returnType = MicrokitTypeUtil.rustBoolType

          st"""unsafe {
              |  return $externCMethodName();
              |}"""
      }
    return RAST.FnImpl(
      visibility = RAST.Visibility.Public,
      sig = RAST.FnSig(
        ident = RAST.IdentString(unsafeMethodName),
        fnDecl = RAST.FnDecl(
          inputs = ISZ(),
          outputs = RAST.FnRetTyImpl(returnType)),
        verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(), contract = None(), meta = ISZ(),
      body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(body)))))
  }

  @pure def getApiContract(srcPort: AadlPort, srcThread: AadlThread): ISZ[RAST.Expr] = {
    var r: ISZ[RAST.Expr] = ISZ()
    for (otherPort <- srcThread.getPorts()) {
      if (srcPort.path == otherPort.path) {
        if (srcPort.direction == Direction.Out) {
          srcPort match {
            case i: AadlEventPort =>
              r = r :+ RAST.ExprST(st"self.${srcPort.identifier} == ${CRustApiPlugin.apiParameterName}")
            case i: AadlDataPort =>
              r = r :+ RAST.ExprST(st"self.${srcPort.identifier} == ${CRustApiPlugin.apiParameterName}")
            case i: AadlEventDataPort =>
              r = r :+ RAST.ExprST(st"self.${srcPort.identifier} == Some(${CRustApiPlugin.apiParameterName})")
          }
        } else {
          r = r :+ RAST.ExprST(st"old(self).${otherPort.identifier} == self.${otherPort.identifier}")
          r = r :+ RAST.ExprST(st"${CRustApiPlugin.apiResultName} == self.${srcPort.identifier}")
        }
      } else {
        r = r :+ RAST.ExprST(st"old(self).${otherPort.identifier} == self.${otherPort.identifier}")
      }
    }
    return r
  }

  @pure def getApiDefaultPutter(srcThread: AadlThread, srcPort: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RAST.Item = {
    val methodName = s"put_${srcPort.identifier}"
    val unverifiedMethodName = s"unverified_$methodName"
    val ghostName = getGhostName(srcPort)
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val ensures = getApiContract(srcPort, srcThread)
    val bodyGhost: RAST.BodyItem =
      srcPort match {
        case i: AadlEventPort => RAST.BodyItemST(st"self.${ghostName} = Some(${CRustApiPlugin.apiParameterName});")
        case i: AadlDataPort => RAST.BodyItemST(st"self.${ghostName} = ${CRustApiPlugin.apiParameterName};")
        case i: AadlEventDataPort => RAST.BodyItemST(st"self.${ghostName} = Some(${CRustApiPlugin.apiParameterName});")
      }
    return RAST.FnImpl(
      sig = RAST.FnSig(
        ident = RAST.IdentString(methodName),
        fnDecl = RAST.FnDecl(
          inputs = ISZ(
            RAST.ParamFixMe(st"&mut self"),
            RAST.ParamImpl(ident = RAST.IdentString(CRustApiPlugin.apiParameterName), kind = RAST.TyPath(ISZ(portTypeNP.qualifiedRustNameS), Some(aadlType.classifier)))),
          outputs = RAST.FnRetTyDefault()),
        verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
      contract = Some(RAST.FnContract(
        optEnsuresMarker = None(),
        ensures = ensures,
        optRequiresMarker = None(),
        requires = ISZ())),
      comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
      body = Some(RAST.MethodBody(
        ISZ(
          RAST.BodyItemST(st"self.api.${unverifiedMethodName}(${CRustApiPlugin.apiParameterName});"),
          bodyGhost))))
  }

  @pure def getApiDefaultGetter(thread: AadlThread, port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RAST.Item = {
    val methodName = s"get_${port.identifier}"
    val unverifiedMethodName = s"unverified_$methodName"
    val ghostName = getGhostName(port)
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val ensures = getApiContract(port, thread)
    val retType: RAST.FnRetTy =
      port match {
        case i: AadlDataPort => RAST.FnRetTyImpl(RAST.TyTuple(ISZ(
          RAST.TyFixMe(st"${CRustApiPlugin.apiResultName} : ${portTypeNP.qualifiedRustName}"))))
        case i: AadlEventPort => RAST.FnRetTyImpl(RAST.TyTuple(ISZ(
          RAST.TyFixMe(st"${CRustApiPlugin.apiResultName} : ${portTypeNP.qualifiedRustName}"))))
        case i: AadlEventDataPort => RAST.FnRetTyImpl(RAST.TyTuple(ISZ(
          RAST.TyFixMe(st"${CRustApiPlugin.apiResultName} : Option<${portTypeNP.qualifiedRustName}>"))))
      }
    return RAST.FnImpl(
      sig = RAST.FnSig(
        ident = RAST.IdentString(methodName),
        fnDecl = RAST.FnDecl(
          inputs = ISZ(
            RAST.ParamFixMe(st"&mut self")),
          outputs = retType),
        verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
      contract = Some(RAST.FnContract(
        optEnsuresMarker = None(),
        ensures = ensures,
        optRequiresMarker = None(),
        requires = ISZ())),
      comments = ISZ(), attributes = ISZ(), visibility = RAST.Visibility.Public, meta = ISZ(),
      body = Some(RAST.MethodBody(
        ISZ(RAST.BodyItemST(st"self.api.${unverifiedMethodName}(&Ghost(self.${ghostName}))")))))
  }

  @pure def getBridgePutApi(port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RAST.Item = {
    val methodName = s"unverified_put_${port.identifier}"
    val unsafeMethodName = s"unsafe_put_${port.identifier}"
    val isEventPort = port.isInstanceOf[AadlEventPort]
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val args: ISZ[RAST.Param] =
      if (isEventPort) ISZ()
      else ISZ(
        RAST.ParamFixMe(st"&mut self"),
        RAST.ParamImpl(
          ident = RAST.IdentString(CRustApiPlugin.apiParameterName),
          kind = RAST.TyPath(ISZ(portTypeNP.qualifiedRustNameS), Some(aadlType.classifier))))
    val body: RAST.BodyItem =
      if (isEventPort) RAST.BodyItemST(st"extern_api::$unsafeMethodName();")
      else RAST.BodyItemST(st"extern_api::$unsafeMethodName(&${CRustApiPlugin.apiParameterName});")
    return RAST.FnImpl(
      attributes = ISZ(RAST.AttributeST(F, st"verifier::external_body")),
      sig = RAST.FnSig(
        ident = RAST.IdentString(methodName),
        fnDecl = RAST.FnDecl(args, RAST.FnRetTyDefault()),
        verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
      comments = ISZ(), visibility = RAST.Visibility.Private, contract = None(), meta = ISZ(),
      body = Some(RAST.MethodBody(ISZ(body))))
  }

  @pure def getBridgeGetApi(port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RAST.FnImpl = {
    val methodName = s"unverified_get_${port.identifier}"
    val unsafeMethodName = s"unsafe_get_${port.identifier}"
    val isEventPort = port.isInstanceOf[AadlEventPort]
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val args: ISZ[RAST.Param] =
      port match {
        case i: AadlEventPort => ISZ()
        case i: AadlDataPort => ISZ(
          RAST.ParamFixMe(st"&mut self"),
          RAST.ParamImpl(
            ident = RAST.IdentString(CRustApiPlugin.apiParameterName),
            kind = RAST.TyRef(
              lifetime = None(),
              mutty = RAST.MutTy(
                ty = RAST.TyPath(ISZ(ISZ("Ghost"), portTypeNP.qualifiedRustNameS), Some(aadlType.classifier)),
                mutbl = RAST.Mutability.Not))))
        case i: AadlEventDataPort => ISZ(
          RAST.ParamFixMe(st"&mut self"),
          RAST.ParamImpl(
            ident = RAST.IdentString(CRustApiPlugin.apiParameterName),
            kind = RAST.TyRef(
              lifetime = None(),
              mutty = RAST.MutTy(
                ty = RAST.TyPath(ISZ(ISZ("Ghost"), ISZ("Option"), portTypeNP.qualifiedRustNameS), Some(aadlType.classifier)),
                mutbl = RAST.Mutability.Not))))
      }
    val retType: RAST.FnRetTy =
      port match {
        case i: AadlEventPort => RAST.FnRetTyImpl(
          RAST.TyTuple(ISZ(RAST.TyFixMe(st"${CRustApiPlugin.apiResultName} : bool"))))
        case i: AadlDataPort => RAST.FnRetTyImpl(
          RAST.TyTuple(ISZ(RAST.TyFixMe(st"${CRustApiPlugin.apiResultName} : ${portTypeNP.qualifiedRustName}"))))
        case i: AadlEventDataPort => RAST.FnRetTyImpl(
          RAST.TyTuple(ISZ(RAST.TyFixMe(st"${CRustApiPlugin.apiResultName} : Option<${portTypeNP.qualifiedRustName}>"))))
      }

    val body: RAST.BodyItem =
      if (isEventPort) RAST.BodyItemST(st"extern_api::$unsafeMethodName();")
      else RAST.BodyItemST(st"return extern_api::$unsafeMethodName();")
    return RAST.FnImpl(
      attributes = ISZ(RAST.AttributeST(F, st"verifier::external_body")),
      sig = RAST.FnSig(
        ident = RAST.IdentString(methodName),
        fnDecl = RAST.FnDecl(
          inputs = args,
          outputs = retType),
        verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
      // TODO: probably move all verus to gumbo plug
      contract = Some(RAST.FnContract(
        optRequiresMarker = None(),
        requires = ISZ(),
        optEnsuresMarker = None(),
        ensures = ISZ(RAST.ExprST(st"${CRustApiPlugin.apiResultName} == ${CRustApiPlugin.apiParameterName}@")))),
      comments = ISZ(), visibility = RAST.Visibility.Private, meta = ISZ(),
      body = Some(RAST.MethodBody(ISZ(body))))
  }


  @pure def getCrustTestingArtifacts(p: AadlPort,
                                     aadlType: ISZ[String],
                                     portTypeNameProvider: CRustTypeNameProvider): (RAST.Item, RAST.Item) = {
    if (p.direction == Direction.In) {
      val varName = s"IN_${p.identifier}"
      val externApiBody: ST =
        if (p.isEvent) {
          st"""unsafe {
              |  match *$varName.lock().unwrap_or_else(|e| e.into_inner()) {
              |    Some(v) => {
              |      *value = v;
              |      return true;
              |    },
              |    None => return false,
              |  }
              |}"""
        } else {
          st"""unsafe {
              |  let guard = $varName.lock().unwrap_or_else(|e| e.into_inner());
              |  *value = guard.expect("Not expecting None");
              |  true
              |}"""
        }

      val externApiTestVariable = RAST.ItemStatic(
        ident = RAST.IdentString(varName),
        visibility = RAST.Visibility.Public,
        ty = RAST.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
        mutability = RAST.Mutability.Not,
        expr = RAST.ExprST(st"Mutex::new(None);"))

      val externApiMethod = RAST.FnImpl(
        attributes = ISZ(RAST.AttributeST(F, st"cfg(test)")),
        sig = RAST.FnSig(
          ident = RAST.IdentString(s"get_${p.identifier}"),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(RAST.ParamImpl(
              ident = RAST.IdentString("value"),
              kind = RAST.TyPtr(mutty =
                RAST.MutTy(
                  ty = RAST.TyPath(ISZ(portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
                  mutbl = RAST.Mutability.Mut)))),
            outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(externApiBody)))))

      return (externApiTestVariable, externApiMethod)

    } else {
      val varName = s"OUT_${p.identifier}"
      val externApiTestVariable = RAST.ItemStatic(
        ident = RAST.IdentString(varName),
        visibility = RAST.Visibility.Public,
        ty = RAST.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
        mutability = RAST.Mutability.Not,
        expr = RAST.ExprST(st"Mutex::new(None);"))
      val externApiMethod = RAST.FnImpl(
        attributes = ISZ(RAST.AttributeST(F, st"cfg(test)")),
        sig = RAST.FnSig(
          ident = RAST.IdentString(s"put_${p.identifier}"),
          fnDecl = RAST.FnDecl(
            inputs = ISZ(RAST.ParamImpl(
              ident = RAST.IdentString("value"),
              kind = RAST.TyPtr(mutty =
                RAST.MutTy(
                  ty = RAST.TyPath(ISZ(portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
                  mutbl = RAST.Mutability.Mut)))),
            outputs = RAST.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RAST.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RAST.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RAST.MethodBody(ISZ(RAST.BodyItemST(
          st"""unsafe {
              |  *$varName.lock().unwrap_or_else(|e| e.into_inner()) = Some(*value);
              |  return true;
              |}""")))))

      return (externApiTestVariable, externApiMethod)
    }
  }
}