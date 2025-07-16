// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.apis

import org.sireum._
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureEvent, AadlPort, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypeNameProvider, CRustTypeProvider}
import org.sireum.hamr.codegen.microkit.{rust => RustAst}
import org.sireum.hamr.codegen.microkit.types.{MicrokitTypeUtil, QueueTemplate}
import org.sireum.hamr.ir.Direction

object CRustApiUtil {

  def processInPort(dstThread: AadlThread, dstPort: AadlPort,
                    crustTypeProvider: CRustTypeProvider): ComponentApiContributions = {
    val portType: AadlType = crustTypeProvider.getRepresentativeType(MicrokitTypeUtil.getPortType(dstPort))
    val portTypeNameProvider = crustTypeProvider.getTypeNameProvider(portType)

    val testingArtifacts = getCrustTestingArtifacts(dstPort, portType.classifier, portTypeNameProvider)

    return ComponentApiContributions.empty(
      externCApis = ISZ(getCExternMethodSig(dstPort, portType, portTypeNameProvider)),
      unsafeExternCApiWrappers = ISZ(getUnsafeGetWrapper(dstPort, portType, crustTypeProvider)),
      externApiTestMockVariables = ISZ(testingArtifacts._1),
      externApiTestingApis = ISZ(testingArtifacts._2),

      testingApis = ISZ(testingArtifacts._3),

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

    val testingArtifacts = getCrustTestingArtifacts(srcPort, portType.classifier, portTypeNameProvider)

    return ComponentApiContributions.empty(
      externCApis = ISZ(getCExternMethodSig(srcPort, portType, portTypeNameProvider)),
      unsafeExternCApiWrappers = ISZ(getUnsafePutWrapper(srcPort, portType, crustTypeProvider)),
      externApiTestMockVariables = ISZ(testingArtifacts._1),
      externApiTestingApis = ISZ(testingArtifacts._2),

      testingApis = ISZ(testingArtifacts._3),

      unverifiedPutApis = ISZ(getBridgePutApi(srcPort, portType, crustTypeProvider)),
      //getApis = ISZ(),

      appApiDefaultPutters = ISZ(getApiDefaultPutter(srcThread, srcPort, portType, crustTypeProvider)),
      //appApiDefaultGetters = ISZ(),

      ghostVariables = ISZ(getGhostVariable(srcPort, portType, portTypeNameProvider)),
      ghostInitializations = ISZ(getGhostInitializations(srcPort, portType, crustTypeProvider))
    )
  }

  @pure def getGhostInitializations(port: AadlPort, a: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.Item = {
    return (
      if (port.isInstanceOf[AadlDataPort])
      RustAst.ItemST(st"${getGhostName(port)}: ${MicrokitTypeUtil.getCRustTypeDefaultVerusValue(a, crustTypeProvider)}")
      else RustAst.ItemST(st"${getGhostName(port)}: None"))
  }

  @pure def getGhostName(port: AadlPort): String = {
    return port.identifier
  }

  @pure def getGhostVariable(port: AadlPort, aadlType: AadlType, typeNameProvider: CRustTypeNameProvider): RustAst.Item = {
    val ghostName = getGhostName(port)
    val fieldType: ISZ[ISZ[String]] =
      port match {
        case i:AadlEventPort => ISZ(typeNameProvider.qualifiedRustNameS)
        case i:AadlDataPort => ISZ(typeNameProvider.qualifiedRustNameS)
        case i:AadlEventDataPort => ISZ(ISZ("Option"), typeNameProvider.qualifiedRustNameS)
      }
    return RustAst.StructField(
      visibility = RustAst.Visibility.Public,
      isGhost = T,
      ident = RustAst.IdentString(ghostName),
      fieldType = RustAst.TyPath(fieldType, Some(aadlType.classifier)))
  }

  @pure def getCExternMethodSig(port: AadlPort, aadlType: AadlType, crustTypeNameProvider: CRustTypeNameProvider): RustAst.FnSig = {
    val methodName: String =
      if (port.direction == Direction.In) s"get_${port.identifier}"
      else s"put_${port.identifier}"
    val args: ISZ[RustAst.Param] =
      if (port.isInstanceOf[AadlEventPort]) ISZ()
      else ISZ(
        RustAst.ParamImpl(
          ident = RustAst.IdentString("value"),
          kind = RustAst.TyPtr(
            RustAst.MutTy(
              ty = RustAst.TyPath(ISZ(crustTypeNameProvider.qualifiedRustNameS), Some(aadlType.classifier)),
              mutbl = RustAst.Mutability.Mut))))
    return RustAst.FnSig(
      verusHeader = None(),
      fnHeader = RustAst.FnHeader(F),
      ident = RustAst.IdentString(methodName),
      generics = None(),
      fnDecl = RustAst.FnDecl(inputs = args, outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)))
  }

  @pure def getUnsafePutWrapper(srcPort: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.Fn = {
    val externMethodName = s"put_${srcPort.identifier}"
    val unsafeMethodName = s"unsafe_$externMethodName"
    val tn = crustTypeProvider.getTypeNameProvider(aadlType)
    val inputs: ISZ[RustAst.Param] =
      if (srcPort.isInstanceOf[AadlEventPort]) ISZ()
      else ISZ(
        RustAst.ParamImpl(
          ident = RustAst.IdentString("value"),
          kind = RustAst.TyRef(
            lifetime = None(),
            mutty = RustAst.MutTy(
              ty = RustAst.TyPath(ISZ(tn.qualifiedRustNameS), Some(aadlType.classifier)),
              mutbl = RustAst.Mutability.Not))))
    val body: ST =
      st"""unsafe {
          |  return $externMethodName(value as *const ${tn.qualifiedRustName} as *mut ${tn.qualifiedRustName});
          |}"""
    return RustAst.FnImpl(
      visibility = RustAst.Visibility.Public,
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(unsafeMethodName),
        fnDecl = RustAst.FnDecl(
          inputs = inputs,
          outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)
        ),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(), contract = None(), meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(body)))))
  }

  @pure def getUnsafeGetWrapper(port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.Fn = {
    val tn = crustTypeProvider.getTypeNameProvider(aadlType)
    val externCMethodName = s"get_${port.identifier}"
    val unsafeMethodName = s"unsafe_$externCMethodName"
    val defaultValue = MicrokitTypeUtil.getCRustTypeDefaultValue(aadlType, crustTypeProvider)
    var returnType: RustAst.Ty = MicrokitTypeUtil.rustBoolType
    val body: ST =
      port match {
        case i: AadlEventDataPort =>
          returnType = RustAst.TyPath(ISZ(ISZ("Option"), tn.qualifiedRustNameS), Some(aadlType.classifier))

          st"""unsafe {
              |  let value: *mut ${tn.qualifiedRustName} = &mut $defaultValue;
              |  if ($externCMethodName(value)) {
              |    return Some(*value);
              |  } else {
              |    return None;
              |  }
              |}"""
        case i: AadlDataPort =>
          returnType = RustAst.TyPath(ISZ(tn.qualifiedRustNameS), Some(aadlType.classifier))

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
    return RustAst.FnImpl(
      visibility = RustAst.Visibility.Public,
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(unsafeMethodName),
        fnDecl = RustAst.FnDecl(
          inputs = ISZ(),
          outputs = RustAst.FnRetTyImpl(returnType)),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      comments = ISZ(), attributes = ISZ(), contract = None(), meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(body)))))
  }

  @pure def getApiContract(srcPort: AadlPort, srcThread: AadlThread): ISZ[RustAst.Expr] = {
    var r: ISZ[RustAst.Expr] = ISZ()
    for (otherPort <- srcThread.getPorts()) {
      if (srcPort.path == otherPort.path) {
        if (srcPort.direction == Direction.Out) {
          srcPort match {
            case i: AadlEventPort =>
              r = r :+ RustAst.ExprST(st"self.${srcPort.identifier} == ${CRustApiPlugin.apiParameterName}")
            case i: AadlDataPort =>
              r = r :+ RustAst.ExprST(st"self.${srcPort.identifier} == ${CRustApiPlugin.apiParameterName}")
            case i: AadlEventDataPort =>
              r = r :+ RustAst.ExprST(st"self.${srcPort.identifier} == Some(${CRustApiPlugin.apiParameterName})")
          }
        } else {
          r = r :+ RustAst.ExprST(st"old(self).${otherPort.identifier} == self.${otherPort.identifier}")
          r = r :+ RustAst.ExprST(st"${CRustApiPlugin.apiResultName} == self.${srcPort.identifier}")
        }
      } else {
        r = r :+ RustAst.ExprST(st"old(self).${otherPort.identifier} == self.${otherPort.identifier}")
      }
    }
    return r
  }

  @pure def getApiDefaultPutter(srcThread: AadlThread, srcPort: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.Item = {
    val methodName = s"put_${srcPort.identifier}"
    val unverifiedMethodName = s"unverified_$methodName"
    val ghostName = getGhostName(srcPort)
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val ensures = getApiContract(srcPort, srcThread)
    val bodyGhost: RustAst.BodyItem =
      srcPort match {
        case i: AadlEventPort => RustAst.BodyItemST(st"self.${ghostName} = Some(${CRustApiPlugin.apiParameterName});")
        case i: AadlDataPort => RustAst.BodyItemST(st"self.${ghostName} = ${CRustApiPlugin.apiParameterName};")
        case i: AadlEventDataPort => RustAst.BodyItemST(st"self.${ghostName} = Some(${CRustApiPlugin.apiParameterName});")
      }
    return RustAst.FnImpl(
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(methodName),
        fnDecl = RustAst.FnDecl(
          inputs = ISZ(
            RustAst.ParamFixMe(st"&mut self"),
            RustAst.ParamImpl(ident = RustAst.IdentString(CRustApiPlugin.apiParameterName), kind = RustAst.TyPath(ISZ(portTypeNP.qualifiedRustNameS), Some(aadlType.classifier)))),
          outputs = RustAst.FnRetTyDefault()),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      contract = Some(RustAst.FnContract(
        optEnsuresMarker = None(),
        ensures = ensures,
        optRequiresMarker = None(),
        requires = ISZ())),
      comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, meta = ISZ(),
      body = Some(RustAst.MethodBody(
        ISZ(
          RustAst.BodyItemST(st"self.api.${unverifiedMethodName}(${CRustApiPlugin.apiParameterName});"),
          bodyGhost))))
  }

  @pure def getApiDefaultGetter(thread: AadlThread, port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.Item = {
    val methodName = s"get_${port.identifier}"
    val unverifiedMethodName = s"unverified_$methodName"
    val ghostName = getGhostName(port)
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val ensures = getApiContract(port, thread)
    val retType: RustAst.FnRetTy =
      port match {
        case i: AadlDataPort => RustAst.FnRetTyImpl(RustAst.TyTuple(ISZ(
          RustAst.TyFixMe(st"${CRustApiPlugin.apiResultName} : ${portTypeNP.qualifiedRustName}"))))
        case i: AadlEventPort => RustAst.FnRetTyImpl(RustAst.TyTuple(ISZ(
          RustAst.TyFixMe(st"${CRustApiPlugin.apiResultName} : ${portTypeNP.qualifiedRustName}"))))
        case i: AadlEventDataPort => RustAst.FnRetTyImpl(RustAst.TyTuple(ISZ(
          RustAst.TyFixMe(st"${CRustApiPlugin.apiResultName} : Option<${portTypeNP.qualifiedRustName}>"))))
      }
    return RustAst.FnImpl(
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(methodName),
        fnDecl = RustAst.FnDecl(
          inputs = ISZ(
            RustAst.ParamFixMe(st"&mut self")),
          outputs = retType),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      contract = Some(RustAst.FnContract(
        optEnsuresMarker = None(),
        ensures = ensures,
        optRequiresMarker = None(),
        requires = ISZ())),
      comments = ISZ(), attributes = ISZ(), visibility = RustAst.Visibility.Public, meta = ISZ(),
      body = Some(RustAst.MethodBody(
        ISZ(RustAst.BodyItemST(st"self.api.${unverifiedMethodName}(&Ghost(self.${ghostName}))")))))
  }

  @pure def getBridgePutApi(port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.Item = {
    val methodName = s"unverified_put_${port.identifier}"
    val unsafeMethodName = s"unsafe_put_${port.identifier}"
    val isEventPort = port.isInstanceOf[AadlEventPort]
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val args: ISZ[RustAst.Param] =
      if (isEventPort) ISZ()
      else ISZ(
        RustAst.ParamFixMe(st"&mut self"),
        RustAst.ParamImpl(
          ident = RustAst.IdentString(CRustApiPlugin.apiParameterName),
          kind = RustAst.TyPath(ISZ(portTypeNP.qualifiedRustNameS), Some(aadlType.classifier))))
    val body: RustAst.BodyItem =
      if (isEventPort) RustAst.BodyItemST(st"extern_api::$unsafeMethodName();")
      else RustAst.BodyItemST(st"extern_api::$unsafeMethodName(&${CRustApiPlugin.apiParameterName});")
    return RustAst.FnImpl(
      attributes = ISZ(RustAst.AttributeST(F, st"verifier::external_body")),
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(methodName),
        fnDecl = RustAst.FnDecl(args, RustAst.FnRetTyDefault()),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      comments = ISZ(), visibility = RustAst.Visibility.Private, contract = None(), meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(body))))
  }

  @pure def getBridgeGetApi(port: AadlPort, aadlType: AadlType, crustTypeProvider: CRustTypeProvider): RustAst.FnImpl = {
    val methodName = s"unverified_get_${port.identifier}"
    val unsafeMethodName = s"unsafe_get_${port.identifier}"
    val isEventPort = port.isInstanceOf[AadlEventPort]
    val portTypeNP = crustTypeProvider.getTypeNameProvider(aadlType)
    val args: ISZ[RustAst.Param] =
      port match {
        case i: AadlEventPort => ISZ()
        case i: AadlDataPort => ISZ(
          RustAst.ParamFixMe(st"&mut self"),
          RustAst.ParamImpl(
            ident = RustAst.IdentString(CRustApiPlugin.apiParameterName),
            kind = RustAst.TyRef(
              lifetime = None(),
              mutty = RustAst.MutTy(
                ty = RustAst.TyPath(ISZ(ISZ("Ghost"), portTypeNP.qualifiedRustNameS), Some(aadlType.classifier)),
                mutbl = RustAst.Mutability.Not))))
        case i: AadlEventDataPort => ISZ(
          RustAst.ParamFixMe(st"&mut self"),
          RustAst.ParamImpl(
            ident = RustAst.IdentString(CRustApiPlugin.apiParameterName),
            kind = RustAst.TyRef(
              lifetime = None(),
              mutty = RustAst.MutTy(
                ty = RustAst.TyPath(ISZ(ISZ("Ghost"), ISZ("Option"), portTypeNP.qualifiedRustNameS), Some(aadlType.classifier)),
                mutbl = RustAst.Mutability.Not))))
      }
    val retType: RustAst.FnRetTy =
      port match {
        case i: AadlEventPort => RustAst.FnRetTyImpl(
          RustAst.TyTuple(ISZ(RustAst.TyFixMe(st"${CRustApiPlugin.apiResultName} : bool"))))
        case i: AadlDataPort => RustAst.FnRetTyImpl(
          RustAst.TyTuple(ISZ(RustAst.TyFixMe(st"${CRustApiPlugin.apiResultName} : ${portTypeNP.qualifiedRustName}"))))
        case i: AadlEventDataPort => RustAst.FnRetTyImpl(
          RustAst.TyTuple(ISZ(RustAst.TyFixMe(st"${CRustApiPlugin.apiResultName} : Option<${portTypeNP.qualifiedRustName}>"))))
      }

    val body: RustAst.BodyItem =
      if (isEventPort) RustAst.BodyItemST(st"extern_api::$unsafeMethodName();")
      else RustAst.BodyItemST(st"return extern_api::$unsafeMethodName();")
    return RustAst.FnImpl(
      attributes = ISZ(RustAst.AttributeST(F, st"verifier::external_body")),
      sig = RustAst.FnSig(
        ident = RustAst.IdentString(methodName),
        fnDecl = RustAst.FnDecl(
          inputs = args,
          outputs = retType),
        verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
      // TODO: probably move all verus to gumbo plug
      contract = Some(RustAst.FnContract(
        optRequiresMarker = None(),
        requires = ISZ(),
        optEnsuresMarker = None(),
        ensures = ISZ(RustAst.ExprST(st"${CRustApiPlugin.apiResultName} == ${CRustApiPlugin.apiParameterName}@")))),
      comments = ISZ(), visibility = RustAst.Visibility.Private, meta = ISZ(),
      body = Some(RustAst.MethodBody(ISZ(body))))
  }


  @pure def getCrustTestingArtifacts(p: AadlPort,
                                     aadlType: ISZ[String],
                                     portTypeNameProvider: CRustTypeNameProvider): (RustAst.Item, RustAst.Item, RustAst.Item) = {
    if (p.direction == Direction.In) {
      val varName = s"IN_${p.identifier}"
      val (externApiBody, testingApiBody): (ST, ST) = {
        val eBody: ST = if (p.isEvent) {
          st"""unsafe {
              |  match *$varName.lock().unwrap() {
              |    Some(v) => {
              |      *value = v;
              |      return true;
              |    },
              |    None => return false,
              |  }
              |}"""
        } else {
          st"""unsafe {
              |  *value = $varName.lock().unwrap().expect("Not expecting None");
              |  return true;
              |}"""
        }
        (eBody, st"""*extern_api::$varName.lock().unwrap() = Some(value)""")
      }
      val variable = RustAst.ItemStatic(
        ident = RustAst.IdentString(varName),
        visibility = RustAst.Visibility.Public,
        ty = RustAst.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
        mutability = RustAst.Mutability.Not,
        expr = RustAst.ExprST(st"Mutex::new(None);"))
      val externApiMethod = RustAst.FnImpl(
        attributes = ISZ(RustAst.AttributeST(F, st"cfg(test)")),
        sig = RustAst.FnSig(
          ident = RustAst.IdentString(s"get_${p.identifier}"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(RustAst.ParamImpl(
              ident = RustAst.IdentString("value"),
              kind = RustAst.TyPtr(mutty =
                RustAst.MutTy(
                  ty = RustAst.TyPath(ISZ(portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
                  mutbl = RustAst.Mutability.Mut)))),
            outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(externApiBody)))))
      val testApiMethod = RustAst.FnImpl(
        attributes = ISZ(RustAst.AttributeST(F, st"cfg(test)")),
        visibility = RustAst.Visibility.Public,
        sig = RustAst.FnSig(
          ident = RustAst.IdentString(s"put_${p.identifier}"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(RustAst.ParamImpl(
              ident = RustAst.IdentString("value"),
              kind = RustAst.TyPath(items = ISZ(portTypeNameProvider.qualifiedRustNameS), aadlType = Some(aadlType)))),
            outputs = RustAst.FnRetTyDefault()),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        contract = None(), comments = ISZ(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(testingApiBody)))))
      return (variable, externApiMethod, testApiMethod)
    } else {
      val varName = s"OUT_${p.identifier}"
      val variable = RustAst.ItemStatic(
        ident = RustAst.IdentString(varName),
        visibility = RustAst.Visibility.Public,
        ty = RustAst.TyPath(ISZ(ISZ("Mutex"), ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
        mutability = RustAst.Mutability.Not,
        expr = RustAst.ExprST(st"Mutex::new(None);"))
      val externApiMethod = RustAst.FnImpl(
        attributes = ISZ(RustAst.AttributeST(F, st"cfg(test)")),
        sig = RustAst.FnSig(
          ident = RustAst.IdentString(s"put_${p.identifier}"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(RustAst.ParamImpl(
              ident = RustAst.IdentString("value"),
              kind = RustAst.TyPtr(mutty =
                RustAst.MutTy(
                  ty = RustAst.TyPath(ISZ(portTypeNameProvider.qualifiedRustNameS), Some(aadlType)),
                  mutbl = RustAst.Mutability.Mut)))),
            outputs = RustAst.FnRetTyImpl(MicrokitTypeUtil.rustBoolType)),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        comments = ISZ(), visibility = RustAst.Visibility.Public, contract = None(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(
          st"""unsafe {
              |  *$varName.lock().unwrap() = Some(*value);
              |  return true;
              |}""")))))

      val (testingApiBody, retType): (ST, RustAst.Ty) = {
        if (p.isEvent) {
          (st"return extern_api::$varName.lock().unwrap().clone()", RustAst.TyPath(items = ISZ(ISZ("Option"), portTypeNameProvider.qualifiedRustNameS), aadlType = Some(aadlType)))
        } else {
          (st"""return extern_api::$varName.lock().unwrap().expect("Not expecting None")""", RustAst.TyPath(items = ISZ(portTypeNameProvider.qualifiedRustNameS), aadlType = Some(aadlType)))
        }
      }
      val testApiMethod = RustAst.FnImpl(
        attributes = ISZ(RustAst.AttributeST(F, st"cfg(test)")),
        visibility = RustAst.Visibility.Public,
        sig = RustAst.FnSig(
          ident = RustAst.IdentString(s"get_${p.identifier}"),
          fnDecl = RustAst.FnDecl(
            inputs = ISZ(),
            outputs = RustAst.FnRetTyImpl(retType)),
          verusHeader = None(), fnHeader = RustAst.FnHeader(F), generics = None()),
        contract = None(), comments = ISZ(), meta = ISZ(),
        body = Some(RustAst.MethodBody(ISZ(RustAst.BodyItemST(testingApiBody)))))
      return (variable, externApiMethod, testApiMethod)
    }
  }
}