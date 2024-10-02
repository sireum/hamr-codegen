// #Sireum

package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.ir._

object BlessST {

  val vizObjectName: String = "StateMachineVisualizer"


  @pure def main(packageName: String,
                 imports: ISZ[ST],
                 completeStateEnumName: String,
                 states: ISZ[ST],
                 componentName: String,
                 bridgeName: String,
                 initialState: String,
                 globalVars: ISZ[ST],
                 methods: ISZ[ST],
                 extensions: ISZ[ST],
                 operationalApiType: String,
                 operationalApiReference: ST
                ): ST = {
    assert(imports.isEmpty)
    val ret: ST =
      st"""${CommentTemplate.doNotEditComment_scala}
          |
          |@enum object ${completeStateEnumName} {
          |  ${(states, "\n")}
          |}
          |
          |object ${componentName} {
          |
          |  val api: ${operationalApiType} = ${operationalApiReference}
          |
          |  var currentState : ${completeStateEnumName}.Type = ${completeStateEnumName}.${initialState}
          |
          |  ${if (globalVars.isEmpty) "// no global vars" else st"${(globalVars, "\n")}"}
          |
          |  ${(methods, "\n\n")}
          |
          |
          |  //methods for execution states
          |  def finalise(_api: ${operationalApiType}): Unit = {}
          |
          |  def activate(_api: ${operationalApiType}): Unit = {}
          |
          |  def deactivate(_api: ${operationalApiType}): Unit = {}
          |
          |  def recover(_api: ${operationalApiType}): Unit = {}
          |}
          |
          |${(extensions, "\n")}
          |"""
    return ret
  }

  @pure def method(name: ST,
                   params: ISZ[ST],
                   body: ST,
                   retType: ST): ST = {
    val ret: ST =
      st"""def ${name}(${(params, ", ")}): ${retType} = {
          |  ${body}
          |}"""
    return ret
  }


  @pure def externalObjectSlang(name: ST,
                                methods: ISZ[ST]): ST = {
    val ret: ST =
      st"""@ext object ${name} {
          |  ${(methods, "\n\n")}
          |}"""
    return ret
  }

  @pure def externalObjectJVM(packageName: ST,
                              name: ST,
                              imports: ISZ[ST],
                              methods: ISZ[ST]): ST = {
    val ret: ST =
      st"""package ${packageName}
          |
          |import org.sireum._
          |${(imports, "\n")}
          |
          |object ${name} {
          |  ${(methods, "\n\n")}
          |}
          |"""
    return ret
  }


  @pure def extMethod(name: ST,
                      params: ISZ[ST],
                      retType: ST): ST = {
    return st"def ${name}(${params}): ${retType} = "
  }

  @pure def ifST(ifbranch: (ST, ST), elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {
    val e = elsifs.map((x: (ST, ST)) =>
      st"""else if(${x._1}) {
          |  ${x._2}
          |}
          |""")
    var body =
      st"""if(${ifbranch._1}) {
          |  ${ifbranch._2}
          |}"""

    if (elsifs.nonEmpty) {
      val _e = elsifs.map((x: (ST, ST)) =>
        st"""else if(${x._1}) {
            |  ${x._2}
            |}
            |""")
      body =
        st"""${body}
            |${_e}"""
    }

    if (els.nonEmpty) {
      body =
        st"""${body}
            |else {
            |  ${els.get}
            |}"""
    }

    return body
  }

  @pure def variableDec(varName: String,
                        varType: ST,
                        defaultValue: ST): ST = {
    return st"var ${varName}: ${varType} = ${defaultValue}"
  }

  @pure def portSend(portName: String,
                     arg: ST): ST = {
    return st"api.put_${portName}(${arg})"
  }

  @pure def portFetch(portName: String): ST = {
    return st"api.get_${portName}()"
  }

  @pure def portGet(portName: String): ST = {
    return st"${portFetch(portName)}.get"
  }

  @pure def portQuery(portName: String): ST = {
    // return st"ports.contains(api.${portName}_Id)"  // FIXME transpiler doesn't handle ((Z) => B @pure)
    return st"contains(dispatchedPorts, api.${portName}_Id)"
  }

  @pure def dispatchedPortsDec(): ST = {
    return st"dispatchedPorts : ISZ[art.Art.PortId]"
  }


  @pure def transpilerWorkaroundContains(): ST = {
    val ret: ST =
      st"""def contains(ids: ISZ[art.Art.PortId], id: art.Art.PortId): B = {
          |  for (i <- ids) {
          |    if (i == id) {
          |      return T
          |    }
          |  }
          |  return F
          |}"""
    return ret
  }


  @pure def debugRegister(): ST = {
    return st"art.ArtDebug.setDebugObject(api.id.string, debugObject)"
  }

  @pure def debugObjectAccess(fieldName: String): ST = {
    return st"debugObject.${fieldName}"
  }

  @pure def debugObject(componentName: String,
                        fields: ISZ[ST],
                        names: ISZ[ST]): ST = {
    val vars: ST = st"${(names.map((s: ST) => st"${s} = $${$s}"), "\n|\n|")}"
    val tripleQuote: ST = st"$$$$$$"
    val flds: ST = if (fields.isEmpty) st"// no global vars" else st"${(fields, ",\n")}"

    val ret: ST =
      st"""@record class ${componentName}_DebugObject (
          |  ${flds}
          |) {
          |  override def string: String = {
          |    return  st$tripleQuote$vars$tripleQuote.render
          |  }
          |}
          |"""
    return ret
  }

  @pure def debugObjectDec(componentName: String,
                           initExps: ISZ[ST]): ST = {
    val inits: ST = if (initExps.isEmpty) st"// no global vars" else st"${(initExps, ",\n")}"
    val ret: ST =
      st"""val debugObject : ${componentName}_DebugObject = ${componentName}_DebugObject (
          |  ${inits}
          |)
          |"""
    return ret
  }


  @pure def vizStateType(m: BTSStateCategory.Type): ST = {
    val name: String = m match {
      case BTSStateCategory.Execute => "EXECUTE"
      case BTSStateCategory.Complete => "COMPLETE"
      case BTSStateCategory.Final => "FINAL"
      case BTSStateCategory.Initial => "INITIAL"
    }
    return st"StateType.${name}"
  }

  @pure def vizCreate(stateName: String,
                      desc: ST,
                      stateTypes: ISZ[ST]): ST = {
    return st"""val ${stateName} = State.create("${stateName}", "${desc}", Seq(${(stateTypes, ", ")}))"""
  }

  @pure def vizBuildSm(stateDecs: ISZ[ST], id: String, label: String, initialState: String, states: ISZ[ST], trans: ISZ[ST]): ST = {
    val ret: ST =
      st"""{
          |  ${(stateDecs, "\n")}
          |
          |  addStateMachineView($id, "${label}",
          |    StateMachine
          |      .builder()
          |      .addStates(${(states, ", ")})
          |      ${(trans, "\n")}
          |      .setInitialState(${initialState})
          |      .build())
          |}"""
    return ret
  }

  @pure def vizTrans(src: String, dst: String, name: String): ST = {
    return st""".addTransition(Transition.create(${src}, ${dst}, "${name}"))"""
  }


  @pure def vizPackageName(basePackage: String): ST = {
    return st"$basePackage.util"
  }

  @pure def vizSlangObject(basePackage: String): ST = {
    val ret: ST =
      st"""// #Sireum
          |
          |package ${vizPackageName(basePackage)}
          |
          |import org.sireum._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |@ext object ${vizObjectName} {
          |  def createStateMachines(): Unit = $$
          |
          |  def transition(componentId: art.Art.BridgeId, transitionName: String): Unit = $$
          |}
          |"""
    return ret
  }

  @pure def vizCallCreateStateMachines(basePackage: String): ST = {
    return st"${basePackage}.util.${vizObjectName}.createStateMachines()"
  }

  @pure def vizCallTransition(basePackage: String, transitionName: String): ST = {
    return st"""${basePackage}.util.${vizObjectName}.transition(api.id, "$transitionName")"""
  }

  @pure def vizExtObject(basePackage: String,
                         imports: ISZ[ST],
                         entries: ISZ[ST]
                        ): ST = {
    val ret: ST =
      st"""package ${vizPackageName(basePackage)}
          |
          |${(imports, "\n")}
          |import java.util.concurrent.atomic.AtomicBoolean
          |import ${basePackage}.Arch
          |import org.santos.inspectorgui.fsm.model.{State, StateMachine, StateType, Transition}
          |import javax.swing.SwingUtilities
          |import org.sireum.{IS, Z}
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |object ${vizObjectName}_Ext {
          |
          |  private val blessVisualizer = ${vizBlessVizName()}()
          |  blessVisualizer.init()
          |
          |  var isFirst = new AtomicBoolean(true)
          |
          |  def addStateMachineView(bridgeId: Z, name: org.sireum.String, stateMachine: StateMachine): Unit =
          |    SwingUtilities.invokeAndWait(() => blessVisualizer.addStateMachineView(bridgeId, name.value, stateMachine))
          |
          |  def updateStateMachineView(bridgeId: Z, state: org.sireum.String): Unit = {
          |    var data = ""
          |    if (!art.ArtDebug.getDebugObject[Any](bridgeId.string).isEmpty) {
          |      data = art.ArtDebug.getDebugObject[Any](bridgeId.string).get.toString
          |    }
          |    SwingUtilities.invokeLater(() => blessVisualizer.updateStateMachine(bridgeId, state.value, data))
          |  }
          |
          |  def createStateMachines(): Unit = {
          |    if(isFirst.getAndSet(false)){
          |        ${(entries, "\n")}
          |    }
          |  }
          |
          |  def transition(componentId: art.Art.BridgeId, transitionName: org.sireum.String): Unit = {
          |    updateStateMachineView(componentId, transitionName.value)
          |  }
          |}
          |"""
    return ret
  }

  @pure def vizBlessVizName(): ST = {
    return st"BlessVisualizer"
  }

  @pure def vizBlessViz(basePackage: String): ST = {
    val ret: ST =
      st"""package ${vizPackageName(basePackage)}
          |
          |import javax.swing._
          |import org.santos.inspectorgui.fsm.form.StateMachineViewPanel
          |import org.santos.inspectorgui.fsm.model.{StateMachine, Transition}
          |import org.sireum.{HashMap, Z}
          |
          |import scala.collection.JavaConverters
          |
          |object ${vizBlessVizName()} {
          |  def apply(): ${vizBlessVizName()} = new ${vizBlessVizName()}
          |}
          |
          |class ${vizBlessVizName()} {
          |
          |  var frame: JFrame = _
          |  var tabbedPane: JTabbedPane = _
          |
          |  private var fsmMap: HashMap[Z, StateMachine] = org.sireum.HashMap.empty
          |  private var panelMap: HashMap[Z, StateMachineViewPanel] = org.sireum.HashMap.empty
          |
          |  def init(): Unit = {
          |    initialize()
          |  }
          |
          |  def initialize(): Unit = {
          |    frame = new JFrame()
          |
          |    frame.setBounds(100, 10, 675, 450)
          |
          |    frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
          |
          |    tabbedPane = new JTabbedPane()
          |    frame.getContentPane.add(tabbedPane)
          |  }
          |
          |  def addStateMachineView(bridgeId: Z, name: String, fsm: StateMachine): Unit = {
          |    try {
          |      if (fsmMap.contains(bridgeId)) {
          |        throw new IllegalStateException("Cannot add duplicate bridge view to ${vizBlessVizName()}.")
          |      }
          |      fsmMap = fsmMap + (bridgeId, fsm)
          |      val panel = new StateMachineViewPanel(fsm)
          |      panelMap = panelMap + (bridgeId, panel)
          |      tabbedPane.addTab(name, panel)
          |      frame.pack()
          |      frame.setVisible(true)
          |    } catch {
          |      case t: Throwable => t.printStackTrace()
          |    }
          |  }
          |
          |  def updateStateMachine(bridgeId: Z, state: String, data: String): Unit = {
          |    try {
          |      val fsm = fsmMap.get(bridgeId).get
          |      val transition = findTransitionFromName(state, fsm)
          |
          |      transition match {
          |        case scala.Some(t) => fsm.update(t, data)
          |        case scala.None => panelMap.get(bridgeId).get.beginTimeline() // should occur 0 or 1 times if weird graph start
          |      }
          |    } catch {
          |      case t: Throwable => t.printStackTrace()
          |    }
          |  }
          |
          |  def getCurrentState(bridgeId: Z): String = {
          |    fsmMap.get(bridgeId).get.currentState().toString
          |  }
          |
          |  private def findTransitionFromName(name: String, fsm: StateMachine): scala.Option[Transition] =
          |    toIterator(fsm.getGraph.getEdges).find(_.getName.equals(name))
          |
          |  private def toIterator[T](l: java.util.Collection[T]): Iterator[T] = {
          |    if (l == null) {
          |      return Iterator[T]()
          |    }
          |    JavaConverters.collectionAsScalaIterableConverter(l).asScala.toIterator
          |  }
          |}
          |"""
    return ret
  }
}