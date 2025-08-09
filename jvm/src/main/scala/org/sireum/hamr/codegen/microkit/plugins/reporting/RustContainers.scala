// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.message.Position

object RustContainers {
  @sig trait PosElement {
    @strictpure def pos: Position
  }

  @datatype class RustModule

  @enum object RustFnKind {
    "Spec"
    "Exec"
    "Rust"
  }

  @enum object Visibility {
    "Private"
    "Public"
  }

  @sig trait RustItem extends PosElement

  @datatype class RustFile(val items: ISZ[RustItem]) {

    @memoize def externs: ISZ[RustExtern] = {
      return (for (f <- items.filter(f => f.isInstanceOf[RustExtern])) yield f.asInstanceOf[RustExtern])
    }

    @memoize def functions: HashSMap[String, RustFn] = {
      val funs: ISZ[RustFn] = for (f <- items.filter(f => f.isInstanceOf[RustFn])) yield f.asInstanceOf[RustFn]
      return HashSMap.empty[String, RustFn] ++ (for(f <- funs) yield f.name ~> f)
    }

    @memoize def traits: HashSMap[String, RustTrait] = {
      val ss: ISZ[RustTrait] = for (i <- items.filter(f => f.isInstanceOf[RustTrait])) yield i.asInstanceOf[RustTrait]
      return HashSMap.empty[String, RustTrait] ++ (for (s <- ss) yield s.name ~> s)
    }

    @memoize def structs: HashSMap[String, RustStruct] = {
      val ss: ISZ[RustStruct] = for (i <- items.filter(f => f.isInstanceOf[RustStruct])) yield i.asInstanceOf[RustStruct]
      return HashSMap.empty[String, RustStruct] ++ (for (s <- ss) yield s.name ~> s)
    }

    @memoize def impls: ISZ[RustImpl] = {
      return (for (i <- items.filter(f => f.isInstanceOf[RustImpl])) yield i.asInstanceOf[RustImpl])
    }

    @pure def getImpl(id: String): Option[RustImpl] = {
      return getImplH(None(), id)
    }

    @pure def getImplH(genericParam: Option[GenericParam], id: String): Option[RustImpl] = {
      for (i <- impls if i.genericParam == genericParam && i.name == id) {
        return Some(i)
      }
      return None()
    }
  }

  @datatype class Marker(val id: String,
                         val isBegin: B,
                         val pos: Position) extends PosElement

  @datatype class GumboId(val id: String,
                          val pos: Position) extends PosElement

  @datatype class GumboClause(val id: String,
                              val pos: Position) extends PosElement

  @datatype class VerusBlock(val isRequiresBlock: B,
                             val gumboClauses: ISZ[GumboClause])

  @datatype class RustFn(val name: String,
                         val visibility: Visibility.Type,
                         val isConst: B,
                         val kind: RustFnKind.Type,
                         val requires: Option[VerusBlock],
                         val ensures: Option[VerusBlock],
                         val pos: Position) extends RustItem {

    @pure def getAssumeClausePos(id: String): Position = {
      for (vb <- requires.get.gumboClauses if vb.id == id) {
        return vb.pos
      }
      halt(s"Didn't find assume clause $id in $name")
    }

    @pure def getGuaranteeClausePos(id: String): Position = {
      for (vb <- ensures.get.gumboClauses if vb.id == id) {
        return vb.pos
      }
      println(ensures.get.gumboClauses)
      halt(s"Didn't find guarantee clause $id in $name")
    }

  }

  @datatype class RustField(val name: String,
                            val visibility: Visibility.Type,
                            val isGhost: B,

                            val pos: Position)

  @sig trait RustContainer extends RustItem {
    @strictpure def finalized: B =
      pos.endLine != 0 && pos.endColumn != 0 && pos.length != 0
  }

  @sig trait RustFieldContainer extends RustContainer {
    @strictpure def fields: Map[String, RustField]
  }

  @sig trait RustMethodContainer extends RustContainer {

    @strictpure def methods: Map[String, RustFn]
  }

  @datatype class RustExtern (val name: String,
                              val methods: Map[String, RustFn],
                              val pos: Position) extends RustMethodContainer

  @datatype class RustTrait(val name: String,
                            val methods: Map[String, RustFn],
                            val pos: Position) extends RustMethodContainer

  @datatype class RustStruct(val name: String,
                             val fields: Map[String, RustField],
                             val pos: Position) extends RustFieldContainer

  @datatype class GenericParam(val paramId: String,
                               val traitId: String)

  @datatype class RustImpl(val genericParam: Option[GenericParam],
                           val name: String,
                           val methods: Map[String, RustFn],
                           val pos: Position) extends RustMethodContainer
}