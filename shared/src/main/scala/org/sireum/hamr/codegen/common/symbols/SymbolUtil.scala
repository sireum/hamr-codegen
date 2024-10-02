// #Sireum

package org.sireum.hamr.codegen.common.symbols

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.types.AadlTypes
import org.sireum.hamr.ir
import org.sireum.hamr.ir.{Annex, AnnexLib, MTransformer => MAirTransformer}
import org.sireum.message.Reporter

object SymbolUtil {

  def buildAadlMaps(aadl: ir.Aadl, reporter: Reporter): AadlMaps = {
    val w = Walker()
    for (c <- aadl.components) {
      w.transformComponent(c)
    }
    var airClassifierMap: HashSMap[String, ir.Component] = HashSMap.empty
    for (d <- aadl.dataComponents) {
      airClassifierMap = airClassifierMap + (d.classifier.get.name ~> d)
    }
    var connInst2Conn: Map[ISZ[String], ISZ[ir.Connection]] = Map.empty
    for (ci <- w.connectionInstances) {
      var conns: ISZ[ir.Connection] = ISZ()
      for (cr <- ci.connectionRefs) {
        w.connectionsMap.get(cr.name.name) match {
          case Some(c) => conns = conns :+ c
          case _ =>
            // maybe it's from a feature group?

            val crops = ops.ISZOps(cr.name.name)
            val src: ISZ[String] = {
              val srcops = ops.ISZOps(ci.src.feature.get.name)
              srcops.slice(ci.src.component.name.size, srcops.s.size)
            }
            val dst: ISZ[String] = {
              val dstops = ops.ISZOps(ci.dst.feature.get.name)
              dstops.slice(ci.dst.component.name.size, dstops.s.size)
            }
            val connCand = st"${crops.last}-${(src, "_")}_${(dst, "_")}".render
            val candName = crops.slice(0, crops.s.size - 1) :+ connCand

            w.connectionsMap.get(candName) match {
              case Some(c2) => conns = conns :+ c2
              case _ =>
              /*
              val msg = s"Couldn't find connection reference ${cr.name.name} for connection instance ${ci.name.name}"
              reporter.warn(ci.name.pos, "", msg)
              */
            }
        }
      }
      connInst2Conn = connInst2Conn + (ci.name.name ~> conns)
    }
    val ret = AadlMaps(w.airComponentMap, w.airFeatureMap, airClassifierMap,
      w.connections, w.connectionsMap,
      w.connectionInstances, w.connectionInstancesMap, w.altConnectionInstancesMap,
      connInst2Conn)

    ret.validate()
    return ret;
  }
}


@msig trait AnnexVisitor {
  // calling context is a singleton so allow visitor to reset their state if needed b/w invocations
  def reset: B

  def offerLibraries(annexLibs: ISZ[AnnexLib],
                     symbolTable: SymbolTable,
                     aadlTypes: AadlTypes,
                     reporter: Reporter): ISZ[AnnexLibInfo]

  def offer(context: AadlComponent,
            annex: Annex,
            annexLibs: ISZ[AnnexLibInfo],
            symbolTable: SymbolTable,
            aadlTypes: AadlTypes,
            reporter: Reporter): Option[AnnexClauseInfo]
}

@datatype class AadlMaps(airComponentMap: HashSMap[String, ir.Component],
                         airFeatureMap: HashSMap[String, ir.Feature],
                         airClassifierMap: HashSMap[String, ir.Component],

                         connections: ISZ[ir.Connection],
                         connectionsMap: Map[ISZ[String], ir.Connection],

                         connectionInstances: ISZ[ir.ConnectionInstance],
                         connectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance],
                         altConnectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance],

                         // this map is currently incomplete for the feature groups in pca pump
                         connectionInstanceToConnectionsMap: Map[ISZ[String], ISZ[ir.Connection]]
                        ) {
  def validate(): Unit = {
    for (c <- connections) {
      for (ci <- c.connectionInstances) {
        assert(altConnectionInstancesMap.contains(ci.name), s"Couldn't find conn instance ${ci.name} for ${c.name.name}")
      }
    }
  }
}

@record class Walker() extends MAirTransformer {
  var airComponentMap: HashSMap[String, ir.Component] = HashSMap.empty
  var airFeatureMap: HashSMap[String, ir.Feature] = HashSMap.empty

  var connections: ISZ[ir.Connection] = ISZ()
  var connectionsMap: Map[ISZ[String], ir.Connection] = Map.empty

  var connectionInstances: ISZ[ir.ConnectionInstance] = ISZ()
  var connectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance] = Map.empty
  var altConnectionInstancesMap: Map[ISZ[String], ir.ConnectionInstance] = Map.empty

  override def postComponent(o: ir.Component): MOption[ir.Component] = {
    val id = CommonUtil.getName(o.identifier)
    airComponentMap = airComponentMap + (id ~> o)
    return MNone[ir.Component]()
  }

  override def postFeature(o: ir.Feature): MOption[ir.Feature] = {
    val id = CommonUtil.getName(o.identifier)
    airFeatureMap = airFeatureMap + (id ~> o)
    return MNone[ir.Feature]()
  }

  override def postConnection(o: ir.Connection): MOption[ir.Connection] = {
    connections = connections :+ o
    connectionsMap = connectionsMap + (o.name.name ~> o)
    return MNone[ir.Connection]()
  }

  override def postConnectionInstance(o: ir.ConnectionInstance): MOption[ir.ConnectionInstance] = {
    connectionInstances = connectionInstances :+ o
    connectionInstancesMap = connectionInstancesMap + (o.name.name ~> o)

    var altName: ISZ[String] = ISZ()
    for (n <- o.name.name) {
      val contents = ops.StringOps(n)
      val pos = contents.stringIndexOf(" -> ")
      if (pos > 0) {
        val src = ops.StringOps(contents.substring(0, pos))
        val dst = ops.StringOps(contents.substring(pos + 4, contents.size))

        val srcs = ops.ISZOps(src.split((c: C) => c == '.'))
        val dsts = ops.ISZOps(dst.split((c: C) => c == '.'))

        altName = altName ++ (srcs.slice(0, srcs.s.size - 1))
        altName = altName :+ s"${srcs.last} -> ${dsts.first}"
        altName = altName ++ dsts.tail
      } else {
        altName = altName :+ n
      }
    }

    altConnectionInstancesMap = altConnectionInstancesMap + (altName ~> o)

    return MNone[ir.ConnectionInstance]()
  }
}