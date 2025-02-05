// #Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.CircularQueue.Policy
import org.sireum.hamr.codegen.common.symbols.{AadlFeatureData, GclAnnexClauseInfo, GclAnnexLibInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypeNameProvider, AadlTypes, ArrayType, BaseType, BitType, EnumType, RecordType, SlangType, TypeKind, TypeUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.util.Util
import org.sireum.hamr.codegen.microkit.util.Util.brand
import org.sireum.hamr.ir
import org.sireum.hamr.ir.GclStateVar
import org.sireum.lang.ast.Param
import org.sireum.message.{Position, Reporter}

object MicrokitTypeUtil {

  @sig trait TypeProvider {
    def name: String

    def aadlType: AadlType
  }

  @sig trait cLangTypeProvider extends TypeProvider {
    def name: String = {
      return cQualifiedName
    }

    def cSimpleName: String
    def cQualifiedName: String

    def cTypeDeclaration: Option[ST]

    def cDefaultValue: ST
  }

  @sig trait rustLangTypeProvider extends TypeProvider {

    def name: String = {
      return rustQualifiedName
    }

    def rustName: String
    def rustQualifiedName: String

    def rustTypeDeclaration: Option[ST]

    // future work: T if the rust type is not based off a C type
    @strictpure def pureRust: B = F
  }

  @datatype class cTypeProvider (val aadlType: AadlType,
                                 val cSimpleName: String,
                                 val cQualifiedName: String,
                                 val cTypeDeclaration: Option[ST],
                                 val cDefaultValue: ST) extends cLangTypeProvider

  @datatype class rustTypeProvider(val aadlType: AadlType,
                                   val rustName: String,
                                   val rustQualifiedName: String,
                                   val rustTypeDeclaration: Option[ST]) extends rustLangTypeProvider

  @datatype class UberTypeProvider(val c: cTypeProvider,
                                   val rust: rustTypeProvider)

  val cratesDir: String = "crates"

  val rustTypesDir: String = s"$cratesDir/types"

  val cTypesDir: String = "types"

  val allTypesFilenamePrefix: String = "types"
  val cAllTypesFilename: String = brand(s"${allTypesFilenamePrefix}.h")
  val rustAllTypesFilename: String = brand(s"${allTypesFilenamePrefix}.rs")

  val aadlTypesFilenamePrefix: String = brand("aadl_types")
  val cAadlTypesFilename: String = s"${aadlTypesFilenamePrefix}.h"
  val rustAadlTypesFilename: String = s"${aadlTypesFilenamePrefix}.rs"

  val make_TYPE_OBJS: String = "TYPE_OBJS"

  val eventPortType: AadlType = {
    BaseType(
      classifier = ISZ("Base_Types::Unsigned_8"),
      nameProvider =
      AadlTypeNameProvider(
        basePackageName = "",
        classifier = ISZ("Base_Types::Unsigned_8"),
        enumValues = ISZ(),
        kind = TypeKind.Base),
      container = None(),
      bitSize = Some(8),
      slangType = SlangType.U8)
  }

  val cTypesFilename: String = brand(s"types.h")
  val rustTypesFilename: String = brand(s"types.rs")

  val eventCounterFilenamePrefix: String = brand("event_counter")
  val cEventCounterFilename: String = s"$eventCounterFilenamePrefix.h"
  val rustEventCounterFilename: String = s"$eventCounterFilenamePrefix.rs"

  val eventCounterTypename: String = brand("event_counter_t")

  val cEventCounterContent: ST =
   st"""#pragma once
       |
       |#include <stdint.h>
       |
       |${Util.doNotEdit}
       |
       |typedef _Atomic uintmax_t ${eventCounterTypename};
       |"""

  val rustEventCounterContent: ST =
    st"""${Util.doNotEdit}
        |
        |pub type $eventCounterTypename = cty::uintmax_t;
        |"""

  val rustMicrokitTypesPrefix: String = brand("microkit_types")
  val rustMicrokitTypesFilename: String = s"${rustMicrokitTypesPrefix}.rs"
  val rustMicrokitTypesContent: ST =
    st"""${Util.doNotEdit}
        |
        |pub type microkit_channel = cty::uint32_t;
        |"""

  val rustCargoContent: ST = st"""${Util.safeToEditMakefile}
                                 |
                                 |[package]
                                 |name = "types"
                                 |version = "0.1.0"
                                 |edition = "2021"
                                 |
                                 |[dependencies]
                                 |cty = "0.2.2"
                                 |
                                 |[lib]
                                 |path = "src/sb_types.rs""""

  def getTypeApiContributions(aadlType: AadlType, typeStore: TypeStore, queueSize: Z): TypeApiContributions = {
    val queueElementTypeName = typeStore.getCTypeName(aadlType)
    return DefaultTypeApiContributions(
      aadlType = aadlType,
      simpleFilename = QueueTemplate.getTypeQueueName(queueElementTypeName, queueSize),
      header = QueueTemplate.header(queueElementTypeName, queueSize),
      implementation = QueueTemplate.implementation(aadlType, queueElementTypeName, queueSize))
  }

  def processDatatype(aadlType: AadlType, reporter: Reporter): UberTypeProvider = {

    def processField(fieldType: AadlType): (String, ST, String) = {
      fieldType match {
        case rt: RecordType =>
          val cTypeName = fieldType.nameProvider.qualifiedCTypeName
          val rustTypeName = fieldType.nameProvider.rustTypeName
          return (cTypeName, st"$cTypeName", rustTypeName)
        case b: BaseType =>
          val cName = translateBaseTypeToC(fieldType.name, reporter).get
          val rustName = translateBaseTypeToRust(fieldType.name, reporter).get
          return (cName, st"${cName}", rustName)
        case _ =>
          val cName = fieldType.nameProvider.qualifiedCTypeName
          val rustTypeName = fieldType.nameProvider.rustTypeName
          return (cName, st"$cName", rustTypeName)
      }
    }

    def processArray(cName: String, cBaseType: String, cContainer: String,
                     byteSizeName: String, byteSize: Z, dimName: String, dim: Z,
                     rustTypeName: String, rustQualifiedTypeName: String, rustBaseType: String): (cTypeProvider, rustTypeProvider) = {
      val ctype = cTypeProvider(
        aadlType = aadlType,
        cSimpleName = cName,
        cQualifiedName = cName,
        cTypeDeclaration = Some(st"""#define $byteSizeName $byteSize
                                    |#define $dimName $dim
                                    |
                                    |typedef $cBaseType $cName [$dimName];
                                    |
                                    |typedef
                                    |  struct $cContainer{
                                    |    $cName f;
                                    |  } $cContainer;"""),
        cDefaultValue = st"not yet")

      val rustType = rustTypeProvider(
        aadlType = aadlType,
        rustName = rustTypeName,
        rustQualifiedName = rustQualifiedTypeName,
        rustTypeDeclaration = Some(
          st"""pub const $byteSizeName: usize = $byteSize;
              |pub const $dimName: usize = $dim;
              |
              |pub type $rustTypeName = [$rustBaseType; $dimName];"""))

      return (ctype, rustType)
    }

    aadlType match {
      case rt: RecordType =>
        val cTypeName = rt.nameProvider.qualifiedCTypeName
        val cFields: ISZ[ST] = for (s <- rt.fields.entries) yield st"${processField(s._2)._1} ${s._1};"
          val ctype = cTypeProvider(
            aadlType = aadlType,
            cSimpleName = cTypeName,
            cQualifiedName = cTypeName,
            cTypeDeclaration = Some(st"""typedef struct $cTypeName {
                                        |  ${(cFields, "\n")}
                                        |} $cTypeName;"""),
            cDefaultValue = st"not yet")

        val rustTypeName = rt.nameProvider.rustTypeName
        val rustQualifiedTypeName = rt.nameProvider.qualifiedRustTypeName
        val rustFields: ISZ[ST] = for (s <- rt.fields.entries) yield st"pub ${s._1}: ${processField(s._2)._3}"
        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustQualifiedTypeName,
          rustTypeDeclaration = Some(
            st"""#[repr(C)]
                |#[derive(Debug, Clone, Copy)]
                |pub struct $rustTypeName {
                |  ${(rustFields, ",\n")}
                |}"""))

        return UberTypeProvider(ctype, rustType)

      case b: BaseType =>
        assert (b.name != "Base_Types::String", "This should be an AadlType by now")

        val cTypeName = translateBaseTypeToC(aadlType.name, reporter).get
        val ctype = cTypeProvider(
          aadlType = aadlType,
          cSimpleName = cTypeName,
          cQualifiedName = cTypeName,
          cTypeDeclaration = None(),
          cDefaultValue = st"not yet")

        val rustTypeName = translateBaseTypeToRust(aadlType.name, reporter).get
        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustTypeName,
          rustTypeDeclaration = None())

        return UberTypeProvider(ctype, rustType)

      case e: EnumType =>
        val cTypeName = e.nameProvider.qualifiedCTypeName
        val ctype = cTypeProvider(
          aadlType = aadlType,
          cSimpleName = cTypeName,
          cQualifiedName = cTypeName,
          cTypeDeclaration = Some(st"""typedef
                                      |  enum {${(e.values, ", ")}} $cTypeName;"""),
          cDefaultValue = st"not yet")

        val rustTypeName = e.nameProvider.rustTypeName
        val rustQualifiedTypeName = e.nameProvider.qualifiedRustTypeName
        val rustEnumValues: ISZ[ST] = for(i <- 0 until e.values.size) yield st"${e.values(i)} = $i"
        val rustType = rustTypeProvider(
          aadlType = aadlType,
          rustName = rustTypeName,
          rustQualifiedName = rustQualifiedTypeName,
          rustTypeDeclaration = Some(
            st"""#[repr(C)]
                |#[derive(Debug, Clone, Copy, PartialEq, Eq)]
                |pub enum $rustTypeName {
                |  ${(rustEnumValues, ",\n")}
                |}"""))

        return UberTypeProvider(ctype, rustType)

      case a: ArrayType =>
        val cName = aadlType.nameProvider.qualifiedCTypeName
        val container = s"${cName}_container"

        val cBaseType: String = getC_TypeName(a, reporter)

        assert (a.dimensions.size == 1 && a.dimensions(0) >= 0, "Linter should have disallowed other variants")
        val dim: Z = a.dimensions(0)

        val byteSize: Z = a.bitSize.get / 8 // linter guarantees bit size will be > 0

        val byteSizeName = getArrayStringByteSizeDefineName(a)
        val dimName = getArrayStringDimDefineName(a)

        val rustTypeName = aadlType.nameProvider.rustTypeName
        val rustQualifiedTypeName = aadlType.nameProvider.qualifiedRustTypeName
        val rustBaseType: String = getRust_TypeName(a, reporter)

        val (ctype, rustType) = processArray(
          cName = cName, cBaseType = cBaseType, cContainer = container,
          byteSizeName = byteSizeName, byteSize = byteSize, dimName = dimName, dim = dim,
          rustTypeName = rustTypeName, rustQualifiedTypeName = rustQualifiedTypeName, rustBaseType = rustBaseType
        )

        return UberTypeProvider(ctype, rustType)

      case x => halt(s"Unexpected Type: $x")
    }
  }

  @pure def getArrayStringByteSizeDefineName(a: AadlType): String = {
    assert (a.isInstanceOf[ArrayType] || a.isInstanceOf[BaseType])
    return s"${a.nameProvider.qualifiedCTypeName}_SIZE"
  }

  @pure def getArrayStringDimDefineName(a: AadlType): String = {
    assert (a.isInstanceOf[ArrayType] || a.isInstanceOf[BaseType])
    return s"${a.nameProvider.qualifiedCTypeName}_DIM"
  }

  @pure def isPrimitive(a: AadlType): B = {
    a match {
      case a: EnumType => return T
      case b: BaseType => return T
      case _ => return F
    }
  }

  def getRust_TypeName(a: AadlType, reporter: Reporter): String = {
    val t: AadlType = a match {
      case a: ArrayType => a.baseType
      case _ => a
    }
    return (
      if (isPrimitive(t)) translateBaseTypeToRust(t.name, reporter).get
      else t.nameProvider.qualifiedRustTypeName
    )
  }

  def getC_TypeName(a: AadlType, reporter: Reporter): String = {
    val t: AadlType = a match {
      case a: ArrayType => a.baseType
      case _ => a
    }
    return (
      if (isPrimitive(t)) translateBaseTypeToC(t.name, reporter).get
      else t.nameProvider.qualifiedCTypeName)
  }

  def translateBaseTypeToC(c: String, reporter: Reporter): Option[String] = {
    c match {
      case "Base_Types::Boolean" => return Some("bool")

      case "Base_Types::Integer_8" => return Some(s"int8_t")
      case "Base_Types::Integer_16" => return Some(s"int16_t")
      case "Base_Types::Integer_32" => return Some(s"int32_t")
      case "Base_Types::Integer_64" => return Some(s"int64_t")

      case "Base_Types::Unsigned_8" => return Some(s"uint8_t")
      case "Base_Types::Unsigned_16" => return Some(s"uint16_t")
      case "Base_Types::Unsigned_32" => return Some(s"uint32_t")
      case "Base_Types::Unsigned_64" => return Some(s"uint64_t")

      case "Base_Types::Float_32" => return Some("float")
      case "Base_Types::Float_64" => return Some("double")

      case "Base_Types::Character" => return Some("char")
      case "Base_Types::String" => return Some("String")

      case x =>
        halt(s"Unexpected base types: $x")
    }
  }

  def translateBaseTypeToRust(c: String, reporter: Reporter): Option[String] = {
    c match {
      case "Base_Types::Boolean" => return Some("bool")

      case "Base_Types::Integer_8" => return Some(s"cty::int8_t")
      case "Base_Types::Integer_16" => return Some(s"cty::int16_t")
      case "Base_Types::Integer_32" => return Some(s"cty::int32_t")
      case "Base_Types::Integer_64" => return Some(s"cty::int64_t")

      case "Base_Types::Unsigned_8" => return Some(s"cty::uint8_t")
      case "Base_Types::Unsigned_16" => return Some(s"cty::uint16_t")
      case "Base_Types::Unsigned_32" => return Some(s"cty::uint32_t")
      case "Base_Types::Unsigned_64" => return Some(s"cty::uint64_t")

      case "Base_Types::Float_32" => return Some("cty::c_float")
      case "Base_Types::Float_64" => return Some("cty::c_double")

      case "Base_Types::Character" => return Some("cty::c_char")
      case "Base_Types::String" => return Some("String")

      case x =>
        halt(s"Unexpected base type: $x")
    }
  }

  @pure def getAllTouchedTypes(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): ISZ[AadlType] = {
    var ret: Set[AadlType] = Set.empty

    var maxStringDim: Z = 0
    var maxString: Option[BaseType] = None()
    def add(posOpt: Option[Position], aadlType: AadlType): Unit = {
      aadlType.name match {
        case "Base_Types::Integer" =>
          reporter.error(posOpt, MicrokitCodegen.toolName, "Unbounded Integer is not supported for Microkit")
        case "Base_Types::Float" =>
          reporter.error(posOpt, MicrokitCodegen.toolName, "Unbounded Float is not supported for Microkit")
        case "Base_Types::String" =>
          TypeUtil.getArrayDimensions(aadlType.container.get) match {
            case ISZ() =>
            case ISZ(dim) =>
              if (dim <= 0) {
                reporter.error(None(), MicrokitCodegen.toolName, s"String dimension must be > 0")
              } else {
                if (dim > maxStringDim) {
                  maxStringDim = dim
                  maxString = Some(aadlType.asInstanceOf[BaseType])
                }
              }
            case _ =>
              reporter.error(None(), MicrokitCodegen.toolName, s"Only a single dimension is allowed for Strings")
          }
        // don't add to ret as strings are treated as arrays by introducing an ArrayType
        case _ =>
          aadlType match {
            case t: ArrayType =>
              t.dimensions match {
                case ISZ() =>
                  reporter.error(None(), MicrokitCodegen.toolName, s"Unbounded arrays are not currently supported: ${t.name}")
                case ISZ(dim) =>
                  if (dim <= 0) {
                    reporter.error(None(), MicrokitCodegen.toolName, s"Array dimension must by >= 1: ${t.name}")
                  }
                case _ =>
                  reporter.error(None(), MicrokitCodegen.toolName, s"Multi dimensional arrays are not currently supported: ${t.name}")
              }
              if (t.bitSize.isEmpty || t.bitSize.get <= 0) {
                reporter.error(None(), MicrokitCodegen.toolName, s"Bit size > 0 must be specified for ${t.name}")
              }

              add(posOpt, t.baseType)
            case t: RecordType =>
              for (f <- t.fields.values) {
                add(posOpt, f)
              }
            case _ =>
          }
          ret = ret + aadlType
      }
    }

    for(thread <- symbolTable.getThreads()) {
      for (port <- thread.getPorts()) {
        port match {
          case d: AadlFeatureData => add(port.feature.identifier.pos, d.aadlType)
          case _ =>
        }
      }
    }
    def processState(s: GclStateVar): Unit = {
      add(s.posOpt, aadlTypes.typeMap.get(s.classifier).get)
    }
    def processParam(p: Param): Unit = {
      p.tipe.typedOpt match {
        case Some(typed: org.sireum.lang.ast.Typed.Name) =>
          val name = st"${(typed.ids, "::")}".render
          add(p.id.attr.posOpt, aadlTypes.typeMap.get(name).get)
        case _ =>
      }
    }

    for (annexes <- symbolTable.annexClauseInfos.values; a <- annexes) {
      a match {
        case g: GclAnnexClauseInfo =>
          for (s <- g.annex.state) {
            processState(s)
          }
          for (m <- g.annex.methods; p <- m.method.sig.params) {
            processParam(p)
          }
      }
    }
    for (gclLib <- symbolTable.annexLibInfos) {
      gclLib match {
        case lib: GclAnnexLibInfo =>
          for (m <- lib.annex.methods;
               p <- m.method.sig.params) {
            processParam(p)
          }
        case _ =>
      }
    }

    var optStringType: Option[AadlType] = None()
    maxString match {
      case Some(b)=>
        // +1 for the null character
        val size: Z = 1 + (if (maxStringDim == 0) 100 else maxStringDim)
        var container = b.container.get
        container = container(properties = ir.Property(
          name = ir.Name(ISZ("Data_Model::Dimension"), None()),
          propertyValues = ISZ(ir.UnitProp((size).string, None())),
          appliesTo = ISZ()) +: container.properties.filter(p => p.name.name != ISZ("Data_Model::Dimension")))
        optStringType = Some(ArrayType(
          classifier = ISZ("Base_Types::String"),
          nameProvider = b.nameProvider,
          container = Some(container),
          bitSize = Some(size * 8),
          dimensions = ISZ(size),
          baseType = BaseType(
            classifier = ISZ("Base_Types::Character"),
            nameProvider = b.nameProvider,
            container = None(),
            bitSize = Some(8),
            slangType = SlangType.C)))
        ret = ret + optStringType.get
      case _ =>
    }

    if (ret.isEmpty) {
      return ISZ()
    } else {

      def getRep(t: AadlType): AadlType = {
        for(e <- ret.elements if t.name == e.name) {
          return e
        }
        halt(s"Infeasible: $t")
      }

      var poset = Poset.empty[AadlType]
      for (t <- ret.elements) {
        t match {
          case t: EnumType => poset = poset.addNode(t)
          case t: BaseType => poset = poset.addNode(t)
          case t: BitType => poset = poset.addNode(t)
          case t: ArrayType =>
            val parent = getRep(t.baseType)
            poset = poset.addNode(t)
            poset = poset.addParents(t, ISZ(parent))
          case t: RecordType =>
            val parents: ISZ[AadlType] = for (f <- t.fields.values) yield getRep(f)
            poset = poset.addNode(t)
            poset = poset.addParents(t, parents)
        }
      }
      var inDegrees: Map[AadlType, Z] = Map.empty[AadlType, Z] ++ (for (e <- poset.nodes.keys) yield (e ~> poset.parentsOf(e).size))
      val queue = CircularQueue.create(max = ret.elements.size, default = ret.elements(0), scrub = F, policy = Policy.NoDrop)
      for (root <- poset.rootNodes) {
        queue.enqueue(root)
      }
      var ordered: ISZ[AadlType] = ISZ()
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        ordered = ordered :+ node
        for (child <- poset.childrenOf(node).elements) {
          val update = inDegrees.get(child).get - 1
          inDegrees = inDegrees + child ~> (update)
          if (update == 0) {
            queue.enqueue(child)
          }
        }
      }
      return ordered
    }
  }
}
