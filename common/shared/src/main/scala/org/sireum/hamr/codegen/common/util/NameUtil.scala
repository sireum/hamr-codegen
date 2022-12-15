// #Sireum
package org.sireum.hamr.codegen.common.util

import org.sireum._
import org.sireum.hamr.codegen.common.{ModuleType, StringUtil}
import org.sireum.hamr.ir

object NameUtil {

  def getNameProvider(idPath: ISZ[String],
                      classifier: String,
                      basePackageName: String): NameProvider = {

    val splitClassifier: ISZ[String] = {
      val san = StringUtil.replaceAll(classifier, "::", ":")
      ops.StringOps(san).split(char => char == ':')
    }

    assert(splitClassifier.size > 1, "classifier must at least be of the form '<id>::<id>'")

    return DefaultNameProvider(idPath, splitClassifier, basePackageName)
  }

  def getAirNameProvider(c: ir.Component,
                         basePackageName: String): NameProvider = {
    return getNameProvider(c.identifier.name, c.classifier.get.name, basePackageName)
  }

  @datatype class DefaultNameProvider(val idPath: ISZ[String],
                                      val classifier: ISZ[String],
                                      val basePackage: String) extends NameProvider

  @sig trait NameProvider {

    def idPath: ISZ[String]

    def classifier: ISZ[String]

    def basePackage: String

    def apiSuffix: String = {
      return componentType
    }

    @memoize def aadlQualifiedName: String = {
      return st"${(classifier)}".render
    }

    @memoize def componentType: String = {
      return StringUtil.sanitizeName(ops.ISZOps(classifier).last)
    }

    @memoize def componentSingletonType: String = {
      return s"${componentType}_${instanceShortName}"
    }

    @memoize def componentSingletonTypeQualifiedName: String = {
      return s"${packageName}.${componentSingletonType}"
    }

    @memoize def api: String = {
      return s"${apiSuffix}_Api"
    }

    @memoize def apiInitialization: String = {
      return s"${apiSuffix}_Initialization_Api"
    }

    @memoize def apiInitializationQualifiedName: String = {
      return s"${packageName}.${apiInitialization}"
    }

    @memoize def apiOperational: String = {
      return s"${apiSuffix}_Operational_Api"
    }

    @memoize def apiOperationalQualifiedName: String = {
      return s"${packageName}.${apiOperational}"
    }

    @memoize def bridge: String = {
      return s"${componentSingletonType}_Bridge"
    }

    @memoize def bridgeIdentifier: String = {
      return s"${identifier}Bridge"
    }

    @memoize def bridgeTypeName: String = {
      return s"${packageName}.${bridge}"
    }

    @memoize def aadlPackage: String = {
      return classifier(0)
    }

    @memoize def packageName: String = {
      return s"${basePackage}.${aadlPackage}"
    }

    @memoize def packagePath: String = {
      return s"${basePackage}/${aadlPackage}"
    }

    @memoize def path: ISZ[String] = {
      return ISZ(basePackage, aadlPackage)
    }

    @memoize def identifier: String = {
      return ops.ISZOps(idPath).last
    }

    @memoize def instanceShortName: String = {
      return st"${(ops.ISZOps(idPath).tail, "_")}".render
    }

    @memoize def instanceName: String = {
      return st"${(idPath, "_")}".render
    }

    @memoize def testName: String = {
      return s"${componentSingletonType}_Test"
    }

    @memoize def testApisName: String = {
      return s"${componentSingletonType}_TestApi"
    }


    @memoize def cApiInitialization_Id: String = {
      return "c_initialization_api"
    }

    @memoize def cApiOperational_Id: String = {
      return "c_operational_api"
    }

    @memoize def cArchInstanceName: String = {
      return s"${basePackage}_Arch_${instanceName}"
    }

    @memoize def cPackageName: String = {
      return st"${(path, "_")}".render
    }

    @memoize def cComponentType: String = {
      return s"${cPackageName}_${componentSingletonType}"
    }

    @memoize def cEntryPointAdapterName: String = {
      return s"${componentSingletonType}_adapter"
    }

    @memoize def cEntryPointAdapterQualifiedName: String = {
      return s"${cPackageName}_${cEntryPointAdapterName}"
    }

    @memoize def cInitializationApi: String = {
      return s"${cPackageName}_${componentType}_Initialization_Api"
    }

    @memoize def cInitializationApi_Id: String = {
      return s"${cPackageName}_${componentSingletonType}_Bridge_${cApiInitialization_Id}"
    }

    @memoize def cOperationalApi: String = {
      return s"${cPackageName}_${componentType}_Operational_Api"
    }

    @memoize def cOperationalApi_Id: String = {
      return s"${cPackageName}_${componentSingletonType}_Bridge_${cApiOperational_Id}"
    }

    @memoize def cBridgeEntryPoints: String = {
      return s"${cPackageName}_${componentSingletonType}_Bridge_EntryPoints"
    }


    @memoize def sel4SlangExtensionName: String = {
      return s"${componentSingletonType}_seL4Nix"
    }

    @memoize def sel4SlangExtensionStubName: String = {
      return s"${sel4SlangExtensionName}_Ext"
    }

    @memoize def sel4SlangExtensionQualifiedNameC: String = {
      return s"${cPackageName}_${sel4SlangExtensionName}"
    }


    @memoize def modulePath: ISZ[String] = {
      val o = ops.ISZOps(idPath)
      return o.slice(1, idPath.size - 1)
    }

    @memoize def singleModuleMainPath: ISZ[String] = {
      return modulePath :+ "main"
    }

    @memoize def moduleMainPath(typ: ModuleType.Type): ISZ[String] = {
      return modulePath :+ typ.name :+ "main"
    }

    @memoize def componentEntryPointSingletonName: String = {
      return s"${componentType}_${instanceShortName}_EntryPoints"
    }

    @memoize def componentEntryPointSingletonQualifiedName: String = {
      return s"${packageName}.${componentEntryPointSingletonName}"
    }

    @memoize def componentEntryPointTraitName: String = {
      return s"${componentType}_${instanceShortName}_EntryPointsSig"
    }

    @memoize def componentEntryPointTraitQualifiedName: String = {
      return s"${packageName}.${componentEntryPointTraitName}"
    }

    @memoize def componentEntryPointImplName: String = {
      return s"${componentType}_${instanceShortName}_EntryPointsImpl"
    }

    @memoize def componentEntryPointStub: String = {
      return s"${componentType}_${instanceShortName}_EntryPointsStub"
    }
  }

}
