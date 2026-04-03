// #Sireum
package org.sireum.hamr.codegen.ros2

import org.sireum._
import org.sireum.hamr.ir
import org.sireum.hamr.codegen.common.properties._
import org.sireum.hamr.codegen.common.symbols.{AadlFeatureData, AadlThread, SymbolTable}
import org.sireum.hamr.codegen.common.types.{AadlType, AadlTypes, ArrayType, RecordType, TypeUtil}
import org.sireum.message.{Position, Reporter}

object RosUtil {
  @pure def isMicroRos(aadlThread: AadlThread): B = {
    PropertyUtil.getDiscreetPropertyValue(aadlThread.properties, Hamr_Ros_Properties.HAMR_ROS__RosNodeKind) match {
      case Some(ir.ValueProp("microRos")) => return T
      case _ => return F
    }
  }

  @pure def getTouchedTypes(aadlTypes: AadlTypes, symbolTable: SymbolTable, reporter: Reporter): ISZ[AadlType] = {
    var ret: Set[AadlType] = Set.empty

    def add(posOpt: Option[Position], aadlType: AadlType): Unit = {
      aadlType match {
        case t: ArrayType =>
          add(posOpt, t.baseType)
        case t: RecordType =>
          for (f <- t.fields.values) {
            add(posOpt, f)
          }
        case _ =>
      }

      ret = ret + aadlType
    }

    for (thread <- symbolTable.getThreads();
         port <- thread.getPorts()) {
      port match {
        case d: AadlFeatureData => add(port.feature.identifier.pos, d.aadlType)
        case _ =>
      }
    }

    if (ret.isEmpty) {
      return ISZ()
    }

    return for(typeName <- TypeUtil.orderTypeDependencies(ret.elements)) yield aadlTypes.typeMap.get(typeName).get
  }
}
