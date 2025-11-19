// #Sireum
package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.CircularQueue.Policy
import org.sireum.hamr.codegen.common.properties.{HamrProperties, OsateProperties, PropertyUtil}
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlEventDataPort, AadlEventPort, AadlFeatureData, AadlPort, GclAnnexClauseInfo, GclAnnexLibInfo, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypeNameProvider, AadlTypes, ArraySizeKind, ArrayType, BaseType, BitType, EnumType, RecordType, SlangType, TypeKind, TypeUtil}
import org.sireum.hamr.codegen.microkit.MicrokitCodegen
import org.sireum.hamr.codegen.microkit.connections._
import org.sireum.hamr.codegen.microkit.plugins.types.{CRustTypePlugin, CRustTypeProvider, CTypeProvider}
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil
import org.sireum.hamr.codegen.microkit.util.MicrokitUtil.brand
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{GclMethod, GclStateVar}
import org.sireum.lang.{ast => SAST}
import org.sireum.lang.ast.Param
import org.sireum.message.{Position, Reporter}
import org.sireum.hamr.codegen.microkit.{rust => RAST}

object MicrokitTypeUtil {

  val rustBoolType: RAST.Ty = RAST.TyPath(ISZ(ISZ("bool")), None())

  val cTypesDir: String = "types"

  val allTypesFilenamePrefix: String = "types"
  val cAllTypesFilename: String = brand(s"${allTypesFilenamePrefix}.h")

  val aadlTypesFilenamePrefix: String = brand("aadl_types")
  val cAadlTypesFilename: String = s"${aadlTypesFilenamePrefix}.h"

  val make_TYPE_OBJS: String = "TYPE_OBJS"

  val eventPortTypeName: String = "AADL_EVENT_PORT_TYPE"

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

  val eventCounterFilenamePrefix: String = brand("event_counter")
  val cEventCounterFilename: String = s"$eventCounterFilenamePrefix.h"

  val eventCounterTypename: String = brand("event_counter_t")

  val cEventCounterContent: ST =
   st"""#pragma once
       |
       |#include <stdint.h>
       |
       |${MicrokitUtil.doNotEdit}
       |
       |typedef _Atomic uintmax_t ${eventCounterTypename};
       |"""

  val rustCargoContent: ST = st"""${MicrokitUtil.safeToEditMakefile}
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

  @pure def getCRustTypeDefaultValue(a: AadlType, cRustTypeProvider: CRustTypeProvider): String = {
    cRustTypeProvider.getRepresentativeType(a) match {
      case b: BaseType => return MicrokitTypeUtil.getRustPrimitiveDefaultValue(a.name)
      case _ if a.name == "Base_Types::String" => return "[0; Base_Types::Base_Types_String_DIM_0]"
      case at: ArrayType =>
        assert (at.dimensions.size == 1, "Need to handle multi-dim arrays")
        val np = cRustTypeProvider.getTypeNameProvider(at)
        val dim0 = CRustTypePlugin.getArrayDimName(np, 0)
        val baseTypeDefault = getCRustTypeDefaultValue(at.baseType, cRustTypeProvider)
        val dimConst = st"${(ops.ISZOps(np.qualifiedRustNameS).dropRight(1), "::")}"
        return st"[$baseTypeDefault; $dimConst::$dim0]".render
      case x => return s"${cRustTypeProvider.getTypeNameProvider(x).qualifiedRustName}::default()"
    }
  }

  @pure def getCRustTypeDefaultVerusValue(a: AadlType, cRustTypeProvider: CRustTypeProvider): ST = {
    cRustTypeProvider.getRepresentativeType(a) match {
      case b: BaseType => return st"${MicrokitTypeUtil.getRustPrimitiveDefaultValue(a.name)}"
      case _ if a.name == "Base_Types::String" => return st"[0; Base_Types::String::Base_Types_String_DIM_0]"
      case at: ArrayType =>
        assert (at.dimensions.size == 1, "Need to handle multi-dim arrays")
        val np = cRustTypeProvider.getTypeNameProvider(at)
        val dim0 = CRustTypePlugin.getArrayDimName(np, 0)
        val baseTypeDefault = getCRustTypeDefaultValue(at.baseType, cRustTypeProvider)
        val dimConst = st"${(ops.ISZOps(np.qualifiedRustNameS).dropRight(1), "::")}"
        return st"[$baseTypeDefault; $dimConst::$dim0]"
      case e: EnumType =>
      val np = cRustTypeProvider.getTypeNameProvider(e)
        return st"${np.qualifiedRustName}::${e.values(0)}"
      case r: RecordType =>
        val fields: ISZ[ST] = for (f <- r.fields.entries) yield st"${f._1}: ${getCRustTypeDefaultVerusValue(f._2, cRustTypeProvider)}"
        val np = cRustTypeProvider.getTypeNameProvider(r)
        return st"${np.qualifiedRustName} { ${(fields, ", ")} }"
    }
  }

  def getPortType(p: AadlPort): AadlType = {
    val ret: AadlType = p match {
      case _: AadlEventPort => MicrokitTypeUtil.eventPortType
      case p: AadlEventDataPort => p.aadlType
      case p: AadlDataPort => p.aadlType
      case _ => halt("Infeasible")
    }
    return ret
  }

  def getTypeApiContributions(aadlType: AadlType, CTypeProvider: CTypeProvider, queueSize: Z): TypeApiContributions = {
    val cTypeNameProvider = CTypeProvider.getTypeNameProvider(aadlType)
    val queueElementTypeName = cTypeNameProvider.mangledName
    return DefaultTypeApiContributions(
      aadlType = aadlType,
      simpleFilename = QueueTemplate.getTypeQueueName(queueElementTypeName, queueSize),
      header = QueueTemplate.header(queueElementTypeName, queueSize),
      implementation = QueueTemplate.implementation(aadlType, queueElementTypeName, queueSize, cTypeNameProvider))
  }

  def translateBaseTypeToC(c: String): String = {
    c match {
      case "Base_Types::Boolean" => return "bool"

      case "Base_Types::Integer" => return "intmax_t"

      case "Base_Types::Integer_8" => return s"int8_t"
      case "Base_Types::Integer_16" => return s"int16_t"
      case "Base_Types::Integer_32" => return s"int32_t"
      case "Base_Types::Integer_64" => return s"int64_t"

      case "Base_Types::Unsigned_8" => return s"uint8_t"
      case "Base_Types::Unsigned_16" => return s"uint16_t"
      case "Base_Types::Unsigned_32" => return s"uint32_t"
      case "Base_Types::Unsigned_64" => return s"uint64_t"

      case "Base_Types::Float_32" => return "float"
      case "Base_Types::Float_64" => return "double"

      case "Base_Types::Character" => return "char"

      // Base_Types::String is now substituted for one that is defined in an AadlArray
      //case "Base_Types::String" => return "String"

      case x =>
        halt(s"Unexpected base types: $x")
    }
  }

  def translateBaseTypeToRust(c: String): String = {
    c match {
      case "Base_Types::Boolean" => return "bool"

      case "Base_Types::Integer" => return "usize"

      case "Base_Types::Integer_8" => return s"i8"
      case "Base_Types::Integer_16" => return s"i16"
      case "Base_Types::Integer_32" => return s"i32"
      case "Base_Types::Integer_64" => return s"i64"

      case "Base_Types::Unsigned_8" => return s"u8"
      case "Base_Types::Unsigned_16" => return s"u16"
      case "Base_Types::Unsigned_32" => return s"u32"
      case "Base_Types::Unsigned_64" => return "u64"

      case "Base_Types::Float_32" => return "f32"
      case "Base_Types::Float_64" => return "f64"

      // AADL does not specify Unicode support for Character, so do not use Rust's char,
      // which is a 4-byte Unicode scalar. Instead, treat it as a raw byte (u8)
      case "Base_Types::Character" => return "u8"

      // Base_Types::String is now substituted for one that is defined in an AadlArray
      //case "Base_Types::String" => return "Base_Types::String"

      case x =>
        halt(s"Unexpected base type: $x")
    }
  }

  def getRustPrimitiveDefaultValue(c: String): String = {
    c match {
      case "Base_Types::Boolean" => return "false"

      case "Base_Types::Integer" => return s"0"

      case "Base_Types::Integer_8" => return s"0"
      case "Base_Types::Integer_16" => return s"0"
      case "Base_Types::Integer_32" => return s"0"
      case "Base_Types::Integer_64" => return s"0"

      case "Base_Types::Unsigned_8" => return s"0"
      case "Base_Types::Unsigned_16" => return s"0"
      case "Base_Types::Unsigned_32" => return s"0"
      case "Base_Types::Unsigned_64" => return "0"

      case "Base_Types::Float_32" => return "0.0"
      case "Base_Types::Float_64" => return "0.0"

      // AADL does not specify Unicode support for Character, so do not use Rust's char,
      // which is a 4-byte Unicode scalar. Instead, treat it as a raw byte (u8)
      case "Base_Types::Character" => return "0"

      // Base_Types::String is now substituted for one that is defined in an AadlArray
      //case "Base_Types::String" => return "Base_Types::String"

      case x =>
        halt(s"Unexpected base type: $x")
    }
  }

  @pure def getAadlTypeFromSlangTypeH(slangType: SAST.Typed.Name, aadlTypes: AadlTypes): AadlType = {
    return getAadlTypeFromSlangType(slangType.ids, aadlTypes)
  }

  @pure def getAadlTypeFromSlangType(slangType: ISZ[String], aadlTypes: AadlTypes): AadlType = {
    slangType match {
      case ISZ("org", "sireum", baseType) =>
        baseType match {

          case "C" => return aadlTypes.typeMap.get("Base_Types::Character").get

          case "B" => return aadlTypes.typeMap.get("Base_Types::Boolean").get

          case "Z" => return aadlTypes.typeMap.get("Base_Types::Integer").get

          case "S8" => return aadlTypes.typeMap.get("Base_Types::Integer_8").get
          case "S16" => return aadlTypes.typeMap.get("Base_Types::Integer_16").get
          case "S32" => return aadlTypes.typeMap.get("Base_Types::Integer_32").get
          case "S64" => return aadlTypes.typeMap.get("Base_Types::Integer_64").get

          case "U8" => return aadlTypes.typeMap.get("Base_Types::Unsigned_8").get
          case "U16" => return aadlTypes.typeMap.get("Base_Types::Unsigned_16").get
          case "U32" => return aadlTypes.typeMap.get("Base_Types::Unsigned_32").get
          case "U64" => return aadlTypes.typeMap.get("Base_Types::Unsigned_64").get

          case "F32" => return aadlTypes.typeMap.get("Base_Types::Float_32").get
          case "F64" => return aadlTypes.typeMap.get("Base_Types::Float_64").get

          case x =>
            halt(s"Unexpected base type: $x")

        }
      case _ =>
        if (slangType(slangType.lastIndex) == "Type") {
          return aadlTypes.getTypeByPath(ops.ISZOps(slangType).dropRight(1))
        } else {
          return aadlTypes.getTypeByPath(slangType)
        }
    }
  }

  /** @return a tuple. First is an ordered sequence of types based on their dependencies (e.g. field types
    *         appear before records that use them). Second is a substitution map (e.g. Base_Types::String
    *         will be mapped to an array based version where the dimension is the largest String dimension
    *         seen in the model, default is 100)
    */
  @pure def getAllTouchedTypes(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): (ISZ[String], Map[String, AadlType]) = {
    var ret: Set[AadlType] = Set.empty

    var maxStringDim: Z = 0
    var maxString: Option[BaseType] = None()
    def add(posOpt: Option[Position], aadlType: AadlType): Unit = {
      aadlType.name match {
        case "Base_Types::Float" =>
          reporter.error(posOpt, MicrokitCodegen.toolName, "Unbounded Float is not supported for Microkit")
        case "Base_Types::Integer" =>
          reporter.error(posOpt, MicrokitCodegen.toolName, "Unbounded Integer is not supported for Microkit")
        case "Base_Types::String" =>
          TypeUtil.getArrayDimensions(aadlType.container.get) match {
            case ISZ() =>
            case ISZ(dim) =>
              if (dim > maxStringDim) {
                maxStringDim = dim
                maxString = Some(aadlType.asInstanceOf[BaseType])
              }
            case _ =>
              reporter.error(posOpt, MicrokitCodegen.toolName, s"Only a single dimension is allowed for Strings")
          }
        // don't add to ret as strings are treated as arrays by introducing an ArrayType
        case _ =>
          aadlType match {
            case t: ArrayType =>
              t.kind match {
                case ArraySizeKind.Fixed =>
                case x =>
                  reporter.error(posOpt, MicrokitCodegen.toolName, s"Only Fixed arrays are currently supported: ${t.name} (attach 'HAMR::Array_Size_Kind => Fixed' to the data component)")
              }
              t.dimensions match {
                case ISZ() =>
                  reporter.error(posOpt, MicrokitCodegen.toolName, s"Unbounded arrays are not currently supported: ${t.name}")
                case ISZ(dim) =>
                  if (dim <= 0) {
                    reporter.error(posOpt, MicrokitCodegen.toolName, s"Array dimension must by >= 1: ${t.name}")
                  }
                case _ =>
                  reporter.error(posOpt, MicrokitCodegen.toolName, s"Multi dimensional arrays are not currently supported: ${t.name}")
              }
              PropertyUtil.getUnitPropZ(aadlType.properties, OsateProperties.MEMORY_PROPERTIES__DATA_SIZE) match {
                case None() =>
                  reporter.error(posOpt, MicrokitCodegen.toolName, s"${OsateProperties.MEMORY_PROPERTIES__DATA_SIZE} must be specified for ${t.name}")
                case _ =>
                  PropertyUtil.getUnitPropZ(aadlType.properties, HamrProperties.HAMR__BIT_CODEC_MAX_SIZE) match {
                    case Some(_) =>
                      reporter.error(posOpt, MicrokitCodegen.toolName, s"Microkit codegen does not currently support both ${OsateProperties.MEMORY_PROPERTIES__DATA_SIZE} and ${HamrProperties.HAMR__BIT_CODEC_MAX_SIZE} being specified for ${t.name}")
                    case _ =>
                      if (t.bitSize.isEmpty || t.bitSize.get <= 0) {
                        reporter.error(posOpt, MicrokitCodegen.toolName, s"Bit size > 0 must be specified for ${t.name}")
                      }
                  }
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
    def processType(typed: org.sireum.lang.ast.Typed.Name, posOpt: Option[Position]): Unit = {
      val name = st"${(typed.ids, "::")}".render
      add(posOpt, aadlTypes.typeMap.get(name).get)
    }

    def processParam(p: Param): Unit = {
      p.tipe.typedOpt match {
        case Some(typed: org.sireum.lang.ast.Typed.Name) => processType(typed, p.id.attr.posOpt)
        case _ =>
      }
    }

    def processMethod(m: GclMethod): Unit = {
      m.method.sig.returnType.typedOpt match {
        case Some(t: org.sireum.lang.ast.Typed.Name) => processType(t, m.posOpt)
        case _ =>
      }
      for (p <- m.method.sig.params) {
        processParam(p)
      }
    }

    for (annexes <- symbolTable.annexClauseInfos.values; a <- annexes) {
      a match {
        case g: GclAnnexClauseInfo =>
          for (s <- g.annex.state) {
            processState(s)
          }
          for (m <- g.annex.methods) {
            processMethod(m)
          }
      }
    }
    for (gclLib <- symbolTable.annexLibInfos) {
      gclLib match {
        case lib: GclAnnexLibInfo =>
          for (m <- lib.annex.methods) {
            processMethod(m)
          }
        case _ =>
      }
    }

    var subs: Map[String, AadlType] = Map.empty
    maxString match {
      case Some(b)=>
        // +1 for the null character
        val size: Z = 1 + (if (maxStringDim == 0) 100 else maxStringDim)
        var container = b.container.get
        container = container(properties = ir.Property(
          name = ir.Name(ISZ("Data_Model::Dimension"), None()),
          propertyValues = ISZ(ir.UnitProp((size).string, None())),
          appliesTo = ISZ()) +: container.properties.filter(p => p.name.name != ISZ("Data_Model::Dimension")))
        val arrayString = ArrayType(
          classifier = ISZ("Base_Types", "String"),
          nameProvider = b.nameProvider,
          container = Some(container),
          bitSize = Some(size * 8),
          dimensions = ISZ(size),
          kind = ArraySizeKind.Fixed,
          baseType = BaseType(
            classifier = ISZ("Base_Types", "Character"),
            nameProvider = b.nameProvider,
            container = None(),
            bitSize = Some(8),
            slangType = SlangType.C))
        ret = ret + arrayString
        subs = subs + "Base_Types::String" ~> arrayString
      case _ =>
    }

    if (ret.isEmpty) {
      return (ISZ(), subs)
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
      var ordered: ISZ[String] = ISZ()
      while (queue.nonEmpty) {
        val node = queue.dequeue()
        ordered = ordered :+ node.name
        for (child <- poset.childrenOf(node).elements) {
          val update = inDegrees.get(child).get - 1
          inDegrees = inDegrees + child ~> (update)
          if (update == 0) {
            queue.enqueue(child)
          }
        }
      }
      return (ordered, subs)
    }
  }
}
