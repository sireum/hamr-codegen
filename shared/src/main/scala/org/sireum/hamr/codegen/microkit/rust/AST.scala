// #Sireum
package org.sireum.hamr.codegen.microkit.rust

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.{IdPath, isThread}
import org.sireum.hamr.codegen.common.containers.Marker
import org.sireum.hamr.codegen.microkit.rust.Printers._

object Printers {
  def printItems(items: ISZ[Item], sep: String): Option[ST] = {
    return if (items.nonEmpty) Some(st"${(for(i <- items) yield i.prettyST, sep)}") else None()
  }

  def printVis(v: Visibility.Type): ST = {
    v match {
      case Visibility.Public => return st"pub "
      case Visibility.Private => return st""
    }
  }

  def printAttributes(attributes: ISZ[Attribute]): Option[ST] = {
    return (
      if (attributes.isEmpty) None()
      else Some(st"${(for (a <- attributes) yield a.prettyST, "\n")}"))
  }

  def printComments(comments: ISZ[Comment]): Option[ST] = {
    return (
      if (comments.isEmpty) None()
      else Some(st"${(for(c <- comments) yield c.prettyST, "\n\n")}"))
  }
}

@enum object Visibility {
  "Public"
  "Private"
}

@sig trait Ident {
  @pure def prettyST: ST
}

@datatype class IdentString(val ident: String) extends Ident {
  @pure override def prettyST: ST = {
    return st"$ident"
  }

  @pure override def string: String = {
    return ident
  }
}

@sig trait Item {
  @pure def prettyST: ST

  @pure override def string: String = {
    return prettyST.render
  }
}

@datatype class ItemST(val s: ST) extends Item {
  @pure override def prettyST: ST = {
    return s
  }
}

@datatype class ItemString(val s: String) extends Item {
  @pure override def prettyST: ST = {
    return st"$s"
  }
}

@datatype class ItemStatic(val ident: Ident,

                           val visibility: Visibility.Type,
                           val ty: Ty,
                           val mutability: Mutability.Type,
                           val expr: Expr) extends Item {
  @pure override def prettyST: ST = {
    return st"""${printVis(visibility)}static ${if(mutability == Mutability.Mut) "mut" else "ref"} ${ident.prettyST}: ${ty.prettyST} = ${expr.prettyST}"""
  }
}

@sig trait Comment extends Item

@datatype class CommentRustDoc(comments: ISZ[ST]) extends Comment {
  @pure override def prettyST: ST = {
    val fss = "///"
    return st"${(for(c <- comments) yield st"$fss $c", "\n")}"
  }
}

@datatype class CommentNonDoc(comments: ISZ[ST]) extends Comment {
  @pure override def prettyST: ST = {
    val fss = "//"
    return st"${(for(c <- comments) yield st"$fss $c", "\n")}"
  }
}

@datatype class CommentST(comment: ST) extends Comment {
  @pure override def prettyST: ST = {
    return comment
  }
}

@datatype class Use(val attributes: ISZ[Attribute],
                    val ident: Ident) extends Item {
  override def prettyST: ST = {
    return (
      st"""${printAttributes(attributes)}
          |use ${ident.prettyST};""")
  }
}


@sig trait Attribute extends Item {
  def inner: B
}

@datatype class AttributeST(val inner: B,
                            val content: ST) extends Attribute {
  @pure override def prettyST: ST = {
    return (st"""#${if (inner) "!" else ""}[$content]""")
  }
}

@datatype class MacCall(val macName: String,
                        val items: ISZ[Item]) extends Item {
  @pure override def prettyST: ST = {
    return (
      st"""$macName! {
          |  ${printItems(items, "\n\n")}
          |}""")
  }
}

@datatype class ExternWrapper(val language: String,
                              val items: ISZ[Item]) extends Item {
  @pure override def prettyST: ST = {
    return (
      st"""extern "$language" {
          |  ${printItems(items, ";\n")}${if (items.nonEmpty) ";" else ""}
          |}""")
  }
}

@datatype class EnumDef(val attributes: ISZ[Attribute],
                        val visibility: Visibility.Type,
                        val ident: Ident,
                        val items: ISZ[Item]
                       ) extends Item {
  @pure override def prettyST: ST = {
    return (
      st"""${printAttributes(attributes)}
          |${printVis(visibility)}enum ${ident.prettyST} {
          |  ${printItems(items, "\n")}
          |}""")
  }
}

@datatype class CEnumValue(val visibility: Visibility.Type,
                           val ident: Ident,
                           val value: Ident) extends Item {
  @pure override def prettyST: ST = {
    return st"${printVis(visibility)}${ident.prettyST} = ${value.prettyST},"
  }
}

@datatype class StructDef(val attributes: ISZ[Attribute],
                          val visibility: Visibility.Type,
                          val ident: Ident,
                          val items: ISZ[Item]) extends Item {
  @pure override def prettyST: ST = {
    return (
      st"""${printAttributes(attributes)}
          |${printVis(visibility)}struct ${ident.prettyST} {
          |  ${printItems(items, ",\n")}
          |}""")
  }
}

@datatype class MarkerWrap(val marker: Marker,
                           val items: ISZ[Item],
                           val sep: String) extends Item {
  @pure override def prettyST: ST = {
    return (
    st"""${marker.beginMarker}
        |${(for(i <- items) yield i.prettyST, sep)}
        |${marker.endMarker}""")
  }
}

@datatype class StructField(val visibility: Visibility.Type,
                            val isGhost: B,
                            val ident: Ident,
                            val fieldType: Ty) extends Item {
  @pure override def prettyST: ST = {
    val ghost: String = if (isGhost) "ghost " else ""
    return st"${printVis(visibility)}$ghost${ident.prettyST}: ${fieldType.prettyST}"
  }
}

// TODO arrays are actually type aliases
@datatype class Array(val companions: ISZ[Item],
                      val attributes: ISZ[Attribute],
                      val visibility: Visibility.Type,
                      val ident: Ident,
                      val dims: ISZ[Ident],
                      val elemType: Ty) extends Item {

  @pure override def prettyST: ST = {
    var dimsST = st"[${elemType.prettyST}; ${dims(dims.lastIndex).prettyST}]"
    for (i <- dims.lastIndex - 1 to 0 by -1) {
      dimsST = st"[$dimsST; ${dims(i).prettyST}]"
    }
    return (st"""${printItems(companions, "\n")}
                |
                |${printAttributes(attributes)}
                |${printVis(visibility)}type ${ident.prettyST} = $dimsST;""")
  }
}

@sig trait Impl extends Item

@datatype class ImplBase(val comments: ISZ[Comment],
                         val attributes: ISZ[Attribute],
                         val implIdent: Option[Ident],
                         val forIdent: Ident,
                         val items: ISZ[Item]) extends Impl {
  @pure override def prettyST: ST = {
    val pre: ST = if (implIdent.nonEmpty) st"${implIdent.get.prettyST} for " else st""
    return (
      st"""${printComments(comments)}
          |${printAttributes(attributes)}
          |impl $pre${forIdent.prettyST} {
          |  ${printItems(items, "\n\n")}
          |}""")
  }
}

@enum object Mutability {
  "Mut"
  "Not"
}

@sig trait Ty extends Item

@datatype class TyPath(val items: ISZ[ISZ[String]],
                       val aadlType: Option[ISZ[String]]) extends Ty {
  @pure override def prettyST: ST = {
    var ret = st""
    for (i <- ops.ISZOps(ops.ISZOps(items).drop(1)).reverse) {
      ret = st"<${(i, "::")}$ret>"
    }
    return st"${(items(0), "::")}$ret"
  }
}

@datatype class TyPtr(val mutty: MutTy) extends Ty {
  @pure override def prettyST: ST = {
    return st"${if (mutty.mutbl == Mutability.Mut) "*mut" else "*const"} ${mutty.ty.prettyST}"
  }
}

@datatype class TyRef(val lifetime: Option[Lifetime],
                      val mutty: MutTy) extends Ty {
  @pure override def prettyST: ST = {
    return st"${if (mutty.mutbl == Mutability.Mut) "&mut " else "&"}${mutty.ty.prettyST}"
  }
}

@datatype class TyTuple(val items: ISZ[Ty]) extends Ty {
  @pure override def prettyST: ST = {
    return st"(${(for(i <- items) yield i.prettyST, ", ")})"
  }
}

@datatype class TyFixMe(val prettyST: ST) extends Ty

@sig trait TypeImpl extends Ty {
  def qualifiedNameS: ISZ[String]

  @pure def simpleName: String = {
    return qualifiedNameS(qualifiedNameS.lastIndex)
  }

  @pure def qualifiedName: String = {
    return st"${(qualifiedNameS, "::")}".render
  }
}

@datatype class TypeAadl(val qualifiedNameS: ISZ[String],

                         val aadlTypeName: IdPath) extends TypeImpl {
  @pure override def prettyST: ST = {
    return st"$simpleName"
  }
}

@datatype class TypeRust(val qualifiedNameS: ISZ[String]) extends TypeImpl {
  @pure override def prettyST: ST = {
    return st"$simpleName"
  }
}


@datatype class Lifetime // placeholder

@datatype class MutTy(val ty: Ty,
                      val mutbl: Mutability.Type)

@datatype class FnHeader(val constness: B)

@datatype class FnVerusHeader(val isOpen: B,
                              val isSpec: B) {
  @pure def prettyST: ST = {
    return st"${if (isOpen) "open " else ""}${if(isSpec) "spec " else "proof " }"
  }
}

@sig trait Param extends Item {
  @pure def ident: Ident
}

@datatype class ParamImpl(val ident: Ident,
                          val kind: Ty) extends Param {
  @pure def prettyST: ST = {
    return st"${ident.prettyST}: ${kind.prettyST}"
  }
}

@datatype class ParamFixMe(val prettyST: ST) extends Param {
  @pure override def ident: Ident = {
    halt("don't call this")
  }
}

@sig trait FnRetTy {
  @pure def prettyST: ST
}
@datatype class FnRetTyDefault extends FnRetTy {
  override def prettyST: ST = {
    return st""
  }
}
@datatype class FnRetTyImpl(ty: Ty) extends FnRetTy {
  @pure override def prettyST: ST = {
    return st" -> ${ty.prettyST}"
  }
}
@datatype class FnDecl(val inputs: ISZ[Param],
                       val outputs: FnRetTy) {
  @pure def prettyST: ST = {
    return (
      if (inputs.size <= 1) st"(${(for (a <- inputs) yield a.prettyST, ",\n")})${outputs.prettyST}"
      else
        st"""(
            |  ${(for (a <- inputs) yield a.prettyST, ",\n")})${outputs.prettyST}""")
  }
}

@datatype class FnSig(val verusHeader: Option[FnVerusHeader],
                      val fnHeader: FnHeader,
                      val ident: Ident,
                      val fnDecl: FnDecl,

                      val generics: Option[Generics]) extends Item {
  @pure override def prettyST: ST = {
    val optVerusHeader: Option[ST] =
      if (verusHeader.nonEmpty) Some(verusHeader.get.prettyST)
      else None()
    val header: String = if (fnHeader.constness) "const " else ""
    val optGenerics: Option[ST] = if (generics.isEmpty) None() else Some(generics.get.prettyST)
    return st"""$optVerusHeader${header}fn ${ident.prettyST}$optGenerics${fnDecl.prettyST}"""
  }
}

@sig trait GenericBounds {
  def prettyST: ST
}

//@datatype class GenericBoundsTrait extends GenericBounds
@datatype class GenericBoundFixMe(val prettyST: ST) extends GenericBounds

@datatype class GenericParam(val ident: Ident,
                             val attributes: ISZ[Attribute],
                             val bounds: GenericBounds) {
  @pure def prettyST: ST = {
    return st"<${ident.prettyST}: ${bounds.prettyST}>"
  }
}

@datatype class Generics(val params: GenericParam) extends Item {
  @pure override def prettyST: ST = {
    return params.prettyST
  }
}

@sig trait Fn extends Item {
  @pure def ident: Ident = {
    return sig.ident
  }

  @pure def sig: FnSig

  @pure def contract: Option[FnContract]

  @pure def body: Option[MethodBody]

  @pure def meta: ISZ[Meta]
}

@sig trait Meta

@datatype class MetaOrigin(val origin: IdPath) extends Meta

@datatype class FnImpl(val comments: ISZ[Comment],
                       val attributes: ISZ[Attribute],
                       val visibility: Visibility.Type,

                       //val generics: Option[Generics],
                       val sig: FnSig,
                       val contract: Option[FnContract],
                       val body: Option[MethodBody],

                       val meta: ISZ[Meta]) extends Fn {

  @pure override def prettyST: ST = {
    val contractST: Option[ST] =
      if (contract.nonEmpty) Some(
        st"""
            |  ${contract.get.prettyST}""")
      else None()
    val optBody: Option[ST] =
      if (body.isEmpty) None()
      else Some(
        st"""
            |{
            |  ${body.get.prettyST}
            |}""")
    return (
      st"""${printComments(comments)}
          |${printAttributes(attributes)}
          |${printVis(visibility)}${sig.prettyST}$contractST $optBody""")
  }
}

@datatype class MethodBody(val items: ISZ[BodyItem]) {
  @pure def prettyST: ST = {
    return st"${(for(i <- items) yield i.prettyST, "\n")}"
  }
}

@sig trait BodyItem {
  @pure def prettyST: ST
}

@datatype class BodyItemSelf(items: ISZ[ST]) extends BodyItem {
  @pure override def prettyST: ST = {
    return (st"""Self {
               |  ${(items, "\n")}
               |}""")
  }
}

@datatype class BodyItemST(prettyST: ST) extends BodyItem

@datatype class BodyItemString(str: String) extends BodyItem {
  @pure override def prettyST: ST = {
    return st"$str"
  }
}


@datatype class FnContract(val optRequiresMarker: Option[Marker],
                           val requires: ISZ[Expr],

                           val optEnsuresMarker: Option[Marker],
                           val ensures: ISZ[Expr]) extends Item {

  @pure override def prettyST: ST = {
    val optRequires: Option[ST] =
      if (requires.isEmpty) None()
      else if (optRequiresMarker.isEmpty)
        Some(
          st"""requires
              |  ${(for(r <- requires) yield r.prettyST, ",\n")}""")
      else Some(
        st"""requires
            |  ${optRequiresMarker.get.beginMarker}
            |  ${(for(r <- requires) yield r.prettyST, ",\n")}
            |  ${optRequiresMarker.get.endMarker}""")

    val optEnsures: Option[ST] =
      if (ensures.isEmpty) None()
      else if (optEnsuresMarker.isEmpty)
        Some(
        st"""ensures
            |  ${(for(r <- ensures) yield r.prettyST, ",\n")}""")
      else Some(
        st"""ensures
            |  ${optEnsuresMarker.get.beginMarker}
            |  ${(for(r <- ensures) yield r.prettyST, ",\n")}
            |  ${optEnsuresMarker.get.endMarker}"""
      )
    return (
      st"""$optRequires
          |$optEnsures""")
  }
}

@sig trait Expr {
  def prettyST: ST
}

@datatype class ExprST (val expr: ST) extends Expr {

  @pure override def prettyST: ST = {
    return expr
  }

}