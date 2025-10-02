// #Sireum
package org.sireum.hamr.codegen.microkit.plugins.reporting

import org.sireum._
import org.sireum.message.Position

object CContainers {

  @sig trait PosElement {
    @strictpure def pos: Position
  }

  @sig trait CItem extends PosElement

  @datatype class CFile(val items: ISZ[CItem]) {
    @pure def getField(id: String): CField = {
      for(i <- items) {
        i match {
          case f: CField if f.identifier == id => return f
          case _ =>
        }
      }
      halt(s"didn't find $id")
    }

    @pure def getMethgod(id: String): CMethod = {
      for (i <- items) {
        i match {
          case m: CMethod if m.identifier == id => return m
          case _ =>
        }
      }
      halt(s"didn't find method $id")
    }
  }

  @datatype class CField(val identifier: String,
                         val typ: String,
                         val isVolatile: B,
                         val pos: Position) extends CItem

  @datatype class CMethod(val identifier: String,
                          val returnType: String,
                          val pos: Position) extends CItem
}
