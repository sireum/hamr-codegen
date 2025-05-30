// #Sireum

package org.sireum.hamr.codegen.act.proof

import org.sireum._
import org.sireum.hamr.codegen.act.ast
import org.sireum.hamr.codegen.act.connections.SBConnectionContainer
import org.sireum.hamr.codegen.act.proof.ProofContainer.{CAmkESComponentCategory, CAmkESConnection, CAmkESConnectionType}
import org.sireum.hamr.codegen.act.templates.SMT2Template
import org.sireum.hamr.codegen.act.util.{ActPlatform, Sel4ConnectorTypes}
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.symbols._
import org.sireum.hamr.codegen.common.util.ResourceUtil

object SMT2ProofGen {
  var resources: ISZ[FileResource] = ISZ()

  var pathSep: String = "_"

  def genSmt2Proof(proofContainer: ProofContainer,
                   astObjects: ISZ[ast.ASTObject],
                   sbConnectionContainer: Map[String, SBConnectionContainer],
                   symbolTable: SymbolTable,
                   outputDir: String,
                   hamrPlatform: ActPlatform.Type): ISZ[FileResource] = {
    resources = ISZ()

    val aadlInstances: ISZ[AadlComponent] = {
      val x: ISZ[AadlComponent] = (for (t <- symbolTable.getThreads()) yield
        if (t.toVirtualMachine(symbolTable)) t.getParent(symbolTable) else t)
      (Set.empty[AadlComponent] ++ x).elements
    }

    val (aadlComponents, aadlBoundProcessors, aadlComponentCategories, aadlDispatchProtocols): (ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST]) = {
      var aadlComps: ISZ[ST] = ISZ()
      var aadlCategories: ISZ[ST] = ISZ()
      var aadlBPs: ISZ[ST] = ISZ()
      var aadlDPs: ISZ[ST] = ISZ()
      for (t <- aadlInstances) {
        val pos: Option[ST] =
          t.component.identifier.pos match {
            case Some(pos) => Some(st" declared at ${pos.uriOpt} (${pos.beginLine}, ${pos.beginColumn})")
            case _ => None()
          }
        aadlComps = aadlComps :+ st"(${t.pathAsString(pathSep)})"
        aadlCategories = aadlCategories :+ SMT2Template.aadlComponentCategory(t)

        t match {
          case a: AadlThread =>
            aadlDPs = aadlDPs :+ SMT2Template.aadlDispatchProtocol(t.pathAsString(pathSep), a)

            a.getParent(symbolTable).getBoundProcessor(symbolTable) match {
              case Some(boundProcessor) =>
                assert(boundProcessor.isInstanceOf[AadlProcessor])

                aadlComps = aadlComps :+ st"(${boundProcessor.pathAsString(pathSep)})"
                aadlCategories = aadlCategories :+ SMT2Template.aadlComponentCategory(boundProcessor)
                aadlBPs = aadlBPs :+ SMT2Template.aadlBoundProcessor(a, boundProcessor)
              case _ =>
            }

          case p: AadlProcess =>

            val boundProcessor = p.getBoundProcessor(symbolTable).get.asInstanceOf[AadlVirtualProcessor]

            aadlDPs = aadlDPs :+ SMT2Template.aadlDispatchProtocol(boundProcessor.pathAsString(pathSep), boundProcessor)
            aadlComps = aadlComps :+ st"(${boundProcessor.pathAsString(pathSep)})"
            aadlCategories = aadlCategories :+ SMT2Template.aadlComponentCategory(boundProcessor)
            aadlBPs = aadlBPs :+ SMT2Template.aadlBoundProcessor(p, boundProcessor)

          case _ =>
        }
      }

      (aadlComps, aadlBPs, aadlCategories, aadlDPs)
    }

    val (aadlPorts, aadlPortComponents, aadlFeatureCategories, aadlPortDirection): (ISZ[String], ISZ[ST], ISZ[ST], ISZ[ST]) = {
      var ports: ISZ[String] = ISZ()
      var portComponents: ISZ[ST] = ISZ()
      var portFeaturesCats: ISZ[ST] = ISZ()
      var portDirs: ISZ[ST] = ISZ()

      for (aadlInstance <- aadlInstances) {
        val connectedPorts: ISZ[AadlPort] = {
          var _ports: Set[AadlPort] = Set.empty
          for (port <- aadlInstance.getPorts()) {
            for (sb <- sbConnectionContainer.values if port == sb.srcPort || port == sb.dstPort) {
              _ports = _ports + port
            }
          }
          _ports.elements
        }

        for (port <- connectedPorts) {
          ports = ports :+ s"(${port.pathAsString(pathSep)})"
          portComponents = portComponents :+ SMT2Template.aadlPortComponents(aadlInstance.pathAsString(pathSep), port.pathAsString(pathSep))

          val portType: String = port.feature.category.name
          portFeaturesCats = portFeaturesCats :+ SMT2Template.aadlFeatureCategory(port.pathAsString(pathSep), portType)
          val dir: String = port.feature.direction.name
          portDirs = portDirs :+ SMT2Template.aadlPortDirection(port.pathAsString(pathSep), dir)
        }
      }
      (ports, portComponents, portFeaturesCats, portDirs)
    }

    val aadlConnectionFlowTos: ISZ[ST] = {
      sbConnectionContainer.values.map((m: SBConnectionContainer) => SMT2Template.flowsTo(m.srcPort.pathAsString(pathSep), m.dstPort.pathAsString(pathSep)))
    }

    var fileServer: Option[ST] = None()
    var timeServer: Option[ST] = None()
    var serialServer: Option[ST] = None()
    var periodicDispatcher: Option[ST] = None()
    var pacer: Option[ST] = None()
    var monitors: ISZ[ST] = ISZ()

    val camkesInstances: ISZ[ast.Instance] = {
      var r: ISZ[ast.Instance] = ISZ()
      for (m <- astObjects) {
        m match {
          case a: ast.Assembly => r = r ++ a.composition.instances
          case _ =>
        }
      }
      r
    }

    val camkesComponents: ISZ[ST] = {
      var ret: ISZ[ST] = ISZ()
      for (instance <- camkesInstances) {
        ret = ret :+ st"(${instance.name})"

        proofContainer.camkesComponentTypes.get(instance.component) match {
          case Some(CAmkESComponentCategory.SerialServer) => serialServer = Some(st"(= _component ${instance.name})")
          case Some(CAmkESComponentCategory.FileServer) => fileServer = Some(st"(= _component ${instance.name})")
          case Some(CAmkESComponentCategory.TimeServer) => timeServer = Some(st"(= _component ${instance.name})")
          case Some(CAmkESComponentCategory.PeriodicDispatcher) => periodicDispatcher = Some(st"(= _component ${instance.name})")
          case Some(CAmkESComponentCategory.Pacer) => pacer = Some(st"(= _component ${instance.name})")
          case Some(CAmkESComponentCategory.Monitor) => monitors = monitors :+ st"(= _component ${instance.name})"
          case Some(CAmkESComponentCategory.Refinement) =>
          case Some(CAmkESComponentCategory.VM_Refinement) =>
          case x =>
            eprintln(s"What is ${instance.component} -- ${x} -- ${instance}")
        }
      }
      ret
    }

    var camkesPortComponents: ISZ[ST] = ISZ()
    var portRefinements: ISZ[ST] = ISZ()
    var portVMAuxsEntries: Map[String, ISZ[ST]] = Map.empty

    val camkesPorts: ISZ[ST] = {
      var _camkesPorts: ISZ[ST] = ISZ()
      for (i <- camkesInstances) {
        i.component match {
          case c: ast.Component =>
            def process(f: ast.CAmkESFeature): ST = {
              val portName = s"${i.name}_${f.name}"

              camkesPortComponents = camkesPortComponents :+ SMT2Template.camkesPortComponents(i.name, portName)

              proofContainer.portRefinementTypes.get(i.name) match {
                case Some(featureMap) =>

                  featureMap.get(f) match {
                    case Some(pf: PortRefinement) =>
                      portRefinements = portRefinements :+ SMT2Template.portRefinement(pf.aadlPort.pathAsString(pathSep), portName)
                    case Some(vmpf: PortVMRefinement) =>
                      portRefinements = portRefinements :+ SMT2Template.portRefinement(vmpf.metaPort.aadlPort.pathAsString(pathSep), portName)
                    case Some(pvma: PortVMAux) =>
                      var entries = portVMAuxsEntries.getOrElse(i.name, ISZ[ST]())
                      entries = entries :+ st"(= cp ${portName})"
                      portVMAuxsEntries = portVMAuxsEntries + (i.name ~> entries)
                    case _ =>
                  }
                case _ =>
                  // TODO: handle VMs
                  if (i.name != "pacer") {
                    eprintln(s"Couldn't find ${i.name} in ${proofContainer.portRefinementTypes.keys}")
                  }
              }

              return st"($portName)"
            }

            _camkesPorts = _camkesPorts ++
              (for (z <- c.dataports) yield process(z)) ++
              (for (z <- c.emits) yield process(z)) ++
              (for (z <- c.uses) yield process(z)) ++
              (for (z <- c.consumes) yield process(z)) ++
              (for (z <- c.provides) yield process(z))

          case c: ast.LibraryComponent =>
            for (portName <- c.ports) {
              val qportName = s"${i.name}_${portName}"
              _camkesPorts = _camkesPorts :+ st"(${qportName})"
              camkesPortComponents = camkesPortComponents :+ SMT2Template.camkesPortComponents(i.name, qportName)
            }
        }
      }
      _camkesPorts
    }

    val portVMAuxs: ISZ[ST] =
      portVMAuxsEntries.entries.map(x =>
        st"""(and (= cc ${x._1})
            |     (or ${(x._2, " ")} false))""")

    var camkesDataPortAccessRestrictions: ISZ[ST] = ISZ()
    for (m <- astObjects) {
      m match {
        case a: ast.Assembly =>
          for (s <- a.configuration) {
            s match {
              case ast.DataPortAccessRestriction(comp, port, v, _) =>
                camkesDataPortAccessRestrictions = camkesDataPortAccessRestrictions :+
                  st"(assert (= ${v.name} (select CAmkESAccessRestrictions ${comp}_${port})))"
              case _ =>
            }
          }
        case _ =>
      }
    }

    val (camkesConnections,
    camkesRefinementConnectionTypes,
    camkesConnectionRefinementFlowTos,
    camkesPacingConnections,
    camkesSelfPacingConnections,
    camkesPeriodicDispatchingConnections,
    camkesVMConnections): (ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST], ISZ[ST]) = {

      var _camkesConTypes: ISZ[ST] = ISZ()
      var _camkesCons: ISZ[ST] = ISZ()
      var _camkesFlowsTo: ISZ[ST] = ISZ()
      var _camkesPacingConns: ISZ[ST] = ISZ()
      var _camkesSelfPacingConns: ISZ[ST] = ISZ()
      var _camkesPDConns: ISZ[ST] = ISZ()
      var _camkesVMConns: ISZ[ST] = ISZ()

      var seenConns: Set[CAmkESConnection] = Set.empty
      for (holder <- proofContainer.camkesConnections if !seenConns.contains(holder)) {
        assert(holder.connection.from_ends.size == 1)

        seenConns = seenConns + holder

        val fromEnd: ast.ConnectionEnd = holder.connection.from_ends(0)

        val src = s"${fromEnd.component}_${fromEnd.end}"

        for (dstEnd <- holder.connection.to_ends) {
          val dst = s"${dstEnd.component}_${dstEnd.end}"
          _camkesFlowsTo = _camkesFlowsTo :+ SMT2Template.camkesFlowsTo(holder.connection.name, src, dst)
        }

        _camkesCons = _camkesCons :+ SMT2Template.camkesConnection(holder.connection.name)

        _camkesConTypes = _camkesConTypes :+ SMT2Template.camkesConnectionType(
          holder.connection.name, Sel4ConnectorTypes.byName(holder.connection.connectionType).get)

        holder.connType match {
          case CAmkESConnectionType.Pacing =>
            _camkesPacingConns = _camkesPacingConns :+ st"(= _conn ${holder.connection.name})"
          case CAmkESConnectionType.SelfPacing =>
            _camkesSelfPacingConns = _camkesSelfPacingConns :+ st"(= _conn ${holder.connection.name})"
          case CAmkESConnectionType.PeriodicDispatching =>
            _camkesPDConns = _camkesPDConns :+ st"(= _conn ${holder.connection.name})"
          case CAmkESConnectionType.VM =>
            _camkesVMConns = _camkesVMConns :+ st"(= _conn ${holder.connection.name})"

          case CAmkESConnectionType.Refinement =>
          case CAmkESConnectionType.VMRefinement =>

          case x => halt(s"Unexpected ${x}")
        }
      }
      (_camkesCons, _camkesConTypes, _camkesFlowsTo, _camkesPacingConns, _camkesSelfPacingConns, _camkesPDConns, _camkesVMConns)
    }

    val componentRefinements: ISZ[ST] = proofContainer.camkesInstances.filter((f: (Option[AadlComponent], ast.Instance)) => f._1.nonEmpty).
      map((r: (Option[AadlComponent], ast.Instance)) => SMT2Template.componentRefinement(r._1.get.pathAsString(pathSep), r._2.name))


    def uniquiIfy(sts: ISZ[ST]): ISZ[ST] = {
      return (Set.empty[String] ++ sts.map((st: ST) => st.render)).elements.map((s: String) => st"$s")
    }

    val proof = SMT2Template.proof(
      mode = hamrPlatform,

      aadlComponents = uniquiIfy(aadlComponents),
      aadlBoundProcessors = aadlBoundProcessors,
      aadlComponentCategories = aadlComponentCategories,
      aadlPorts = aadlPorts,
      aadlPortComponents = aadlPortComponents,
      aadlFeatureCategories = aadlFeatureCategories,
      aadlPortDirection = aadlPortDirection,
      aadlConnectionFlowTos = aadlConnectionFlowTos,
      aadlDispatchProtocols = aadlDispatchProtocols,

      camkesComponents = camkesComponents,
      camkesDataPortAccessRestrictions = camkesDataPortAccessRestrictions,
      periodicDispatcherComponent = periodicDispatcher,
      pacerComponent = pacer,
      fileServerComponent = fileServer,
      timeServerComponent = timeServer,
      serialServerComponent = serialServer,

      camkesPorts = camkesPorts,

      camkesPortComponents = camkesPortComponents,
      camkesConnectionTypes = camkesRefinementConnectionTypes,
      camkesConnectionFlowTos = camkesConnectionRefinementFlowTos,

      camkesConnections = camkesConnections,

      selfPacingConnections = camkesSelfPacingConnections,
      pacingConnections = camkesPacingConnections,
      periodicDispatchingConnections = camkesPeriodicDispatchingConnections,
      vmConnections = camkesVMConnections,

      componentRefinements = componentRefinements,
      portRefinements = portRefinements,
      portVMAuxs = portVMAuxs,

      modelSchedulingType = proofContainer.modelSchedulingType
    )

    val path: String = "proof/smt2_case.smt2"

    resources = resources :+ ResourceUtil.createResource(
      path = path,
      content = proof,
      overwrite = T)

    return resources
  }
}
