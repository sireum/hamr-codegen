// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.{Port, ProjectDirectories, Util}
import org.sireum.hamr.codegen.common.CommonUtil
import org.sireum.hamr.codegen.common.containers.FileResource
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider
import org.sireum.hamr.codegen.common.util.ResourceUtil
import org.sireum.hamr.ir.FeatureCategory

object TestTemplate {

  @pure def addId(s: String): String = {
    return s"${s}_Id"
  }

  @pure def bridgeTestSuite(basePackage: String,
                            names: NameProvider): ST = {
    val ret: ST =
      st"""package ${names.packageName}
          |
          |import org.sireum._
          |import ${basePackage}._
          |
          |${CommentTemplate.safeToEditComment_scala}
          |class ${names.testName} extends ${names.testScalaTestName} {
          |
          |  test("Example Unit Test for Initialise Entry Point"){
          |    // Initialise Entry Point doesn't read input port values, so just proceed with
          |    // launching the entry point code
          |    testInitialise()
          |    // use get_XXX methods and check_concrete_output() from test/util/../YYY_TestApi
          |    // retrieve values from output ports and check against expected results
          |  }
          |
          |  test("Example Unit Test for Compute Entry Point"){
          |    // use put_XXX methods from test/util/../YYY_TestApi to seed input ports with values
          |    testCompute()
          |    // use get_XXX methods and check_concrete_output() from test/util/../YYY_TestApi
          |    // retrieve values from output ports and check against expected results
          |  }
          |}
          |"""
    return ret
  }

  @pure def bridgeTestApis(basePackage: String,
                           names: NameProvider,
                           projectDirectories: ProjectDirectories,
                           ports: ISZ[Port]): ISZ[FileResource] = {

    var resources: ISZ[FileResource] = ISZ()
    var concretePutParams: ISZ[ST] = ISZ()
    var concretePutBlocks: ISZ[ST] = ISZ()
    var concretePutScalaDoc: ISZ[ST] = ISZ()

    var concreteCheckParams: ISZ[ST] = ISZ()
    var concreteCheckBlocks: ISZ[ST] = ISZ()
    var concreteCheckScalaDoc: ISZ[ST] = ISZ()

    val setters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isInFeature(p.feature)).map((p: Port) => {
      val putMethodName = s"put_${p.name}"

      val (putParamName, putArgName, concreteParamName, concreteParamType): (String, String, String, String) =
        p.feature.category match {
          case FeatureCategory.EventPort => {
            concretePutScalaDoc = concretePutScalaDoc :+
              st"""* @param ${p.name} the number of events to place in the ${p.name} event port queue.
                  |*   ART currently supports single element event queues so at most
                  |*   one event will be placed in the queue."""

            concretePutBlocks = concretePutBlocks :+
              st"""for(i <- 0 until ${p.name}) {
                  |  ${putMethodName}()
                  |}"""
            (s"", s"art.Empty()", p.name, "Z")
          }
          case _ => {
            val ptype = p.getPortTypeNames.qualifiedReferencedTypeName
            val concreteParamType: String =
              if (CommonUtil.isAadlEventDataPort(p.feature)) s"ISZ[${ptype}]"
              else ptype

            if (CommonUtil.isAadlEventDataPort(p.feature)) {
              concretePutScalaDoc = concretePutScalaDoc :+
                st"""* @param ${p.name} payloads for event data port ${p.name}.
                    |*   ART currently supports single element event data queues so
                    |*   only the last element of ${p.name} will be used"""

              concretePutBlocks = concretePutBlocks :+
                st"""for(v <- ${p.name}){
                    |  ${putMethodName}(v)
                    |}"""

            } else {
              concretePutScalaDoc = concretePutScalaDoc :+
                st"* @param ${p.name} payload for data port ${p.name}"

              concretePutBlocks = concretePutBlocks :+
                st"""${putMethodName}(${p.name})"""
            }

            (s"value : ${p.getPortTypeNames.qualifiedReferencedTypeName}",
              s"${p.getPortTypeNames.qualifiedPayloadName}(value)",
              p.name,
              concreteParamType)
          }
        }

      concretePutParams = concretePutParams :+ st"${concreteParamName} : ${concreteParamType}"

      st"""// setter for in ${p.feature.category}
          |def ${putMethodName}(${putParamName}): Unit = {
          |  Art.insertInInfrastructurePort(${names.archInstanceName}.operational_api.${p.name}_Id, ${putArgName})
          |}
          |"""
    })

    val concretePutter: Option[ST] =
      if (concretePutBlocks.isEmpty) {
        None()
      }
      else {
        val scalaDoc = concretePutScalaDoc.map((m: ST) => st"${m}")
        Some(
          st"""/** helper function to set the values of all input ports.
              | ${(scalaDoc, "\n")}
              | */
              |def put_concrete_inputs(${(concretePutParams, ",\n")}): Unit = {
              |  ${(concretePutBlocks, "\n")}
              |}
              |
              |""")
      }

    val testFailures = "testFailures"

    val getters: ISZ[ST] = ports.filter((p: Port) => CommonUtil.isOutFeature(p.feature)).map((p: Port) => {
      val portName = p.name
      val portNameValue = s"${portName}Value"
      val getterName = s"get_${portName}"
      val payloadGetterName: String = s"get_${portName}_payload()"

      val isEvent = p.feature.category == FeatureCategory.EventPort
      val typeName = p.getPortTypeNames.qualifiedReferencedTypeName
      val payloadType: String = if (isEvent) "art.Empty" else p.getPortTypeNames.qualifiedPayloadName
      val _match: String = if (isEvent) "art.Empty()" else s"${payloadType}(v)"
      val value: String = if (isEvent) "art.Empty()" else "v"

      val (checkParamType, concretePreamble, checkExplanation, checkScalaDoc): (String, ST, String, ST) =
        p.feature.category match {
          case FeatureCategory.EventPort =>
            val preamble =
              st"""// TODO: event port getter should return the number of events in
                  |//       the output queue when queue sizes > 1 support is added to ART
                  |val ${portNameValue}: Z = if(${getterName}().nonEmpty) z"1" else z"0""""
            val scalaDoc =
              st"""* @param ${portName} method that will be called with the number of events to be sent
                  |*        on the outgoing event port '${portName}'."""
            ("Z", preamble, s"$${${portNameValue}} events were in the outgoing event queue", scalaDoc)
          case FeatureCategory.EventDataPort =>
            val preamble =
              st"""var ${portNameValue}: ISZ[${typeName}] = ISZ()
                  |// TODO: event data port getter should return all of the events/payloads
                  |//       received on event data ports when queue sizes > 1 support is added
                  |//       to ART
                  |if(${getterName}().nonEmpty) { ${portNameValue} = ${portNameValue} :+ ${getterName}().get }"""
            val scalaDoc =
              st"""* @param ${portName} method that will be called with the payloads to be sent
                  |*        on the outgoing event data port '${portName}'."""
            (s"ISZ[$typeName]", preamble, s"received $${${portNameValue}.size} events with the following payloads $${${portNameValue}}", scalaDoc)
          case FeatureCategory.DataPort =>
            val preamble = st"val ${portNameValue}: ${typeName} = ${getterName}().get"
            val scalaDoc =
              st"""* @param ${portName} method that will be called with the value of the outgoing data
                  |*        port '${portName}'."""
            (typeName, preamble, s"value of the outgoing data port is $${${portNameValue}}", scalaDoc)
          case _ => halt("Unexpected")
        }

      concreteCheckScalaDoc = concreteCheckScalaDoc :+ checkScalaDoc

      concreteCheckParams = concreteCheckParams :+ st"${portName}: ${checkParamType} => B"

      concreteCheckBlocks = concreteCheckBlocks :+
        st"""${concretePreamble}
            |if(!${portName}(${portNameValue})) {
            |  testFailures = testFailures :+ st"'${portName}' did not match expected: ${checkExplanation}"
            |}"""

      st"""// getter for out ${p.feature.category}
          |def ${getterName}(): Option[${typeName}] = {
          |  val value: Option[${typeName}] = ${payloadGetterName} match {
          |    case Some(${_match}) => Some(${value})
          |    case Some(v) => halt(s"Unexpected payload on port ${portName}.  Expecting '${payloadType}' but received $${v}")
          |    case _ => None[${typeName}]()
          |  }
          |  return value
          |}
          |
          |// payload getter for out ${p.feature.category}
          |def ${payloadGetterName}: Option[${payloadType}] = {
          |  return Art.observeOutInfrastructurePort(${names.archInstanceName}.initialization_api.${addId(portName)}).asInstanceOf[Option[${payloadType}]]
          |}
          |"""
    })

    val concreteChecker: Option[ST] =
      if (concreteCheckBlocks.isEmpty) {
        None()
      }
      else {
        val scalaDoc = concreteCheckScalaDoc.map((m: ST) => st"${m}")
        Some(
          st"""/** helper function to check ${names.componentSingletonType}'s
              | * output ports.  Use named arguments to check subsets of the output ports.
              | ${(scalaDoc, "\n")}
              | */
              |def check_concrete_output(${(concreteCheckParams, ",\n")}): Unit = {
              |  var ${testFailures}: ISZ[ST] = ISZ()
              |
              |  ${(concreteCheckBlocks, "\n")}
              |
              |  assert(testFailures.isEmpty, st"$${(testFailures, "\n")}".render)
              |}
              |
              |""")
      }

    val ret: ST =
      st"""// #Sireum
          |
          |package ${names.packageName}
          |
          |import org.sireum._
          |import art.Art
          |import ${basePackage}._
          |
          |${CommentTemplate.doNotEditComment_scala}
          |@msig trait ${names.testApisName} {
          |
          |  def BeforeEntrypoint(): Unit = {
          |    Art.initTest(${names.archInstanceName})
          |  }
          |
          |  def AfterEntrypoint(): Unit = {
          |    Art.finalizeTest(${names.archInstanceName})
          |  }
          |
          |  def testCompute(): Unit = {
          |    Art.manuallyClearOutput()
          |    Art.testCompute(${names.archInstanceName})
          |  }
          |
          |  def testInitialise(): Unit = {
          |    Art.manuallyClearOutput()
          |    Art.testInitialise(${names.archInstanceName})
          |  }
          |
          |  ${concretePutter}
          |  ${concreteChecker}
          |  ${(setters ++ getters, "\n")}
          |}
          |"""

    resources = resources :+ ResourceUtil.createResource(Util.pathAppend(projectDirectories.testUtilDir, ISZ(names.packagePath, s"${names.testApisName}.scala")), ret, T)
    resources = resources :+ slang2ScalaTestWrapper(projectDirectories, names, None())

    return resources
  }

  def slang2ScalaTestWrapper(projectDirectories: ProjectDirectories, names: NameProvider, altName: Option[(String, String)]): FileResource = {
    val className2Use: String = if (altName.isEmpty) names.testScalaTestName else altName.get._1
    val extendsName2Use: String = if (altName.isEmpty) names.testApisName else altName.get._2
    val ret: ST =
      st"""package ${names.packageName}
          |
          |import org.scalatest.{BeforeAndAfterEach, OneInstancePerTest}
          |import org.scalatest.funsuite.AnyFunSuite
          |import org.sireum.$$internal.MutableMarker
          |
          |${CommentTemplate.doNotEditComment_scala}
          |abstract class ${className2Use} extends
          |  AnyFunSuite with OneInstancePerTest with BeforeAndAfterEach with
          |  ${extendsName2Use} {
          |
          |  var clonable: Boolean = true
          |  var owned: Boolean = false
          |
          |  override def string: org.sireum.String = {
          |    this.toString()
          |  }
          |
          |  override def $$clonable: Boolean = {
          |    return clonable
          |  }
          |
          |  override def $$clonable_=(b: Boolean): MutableMarker = {
          |    clonable = b
          |    return this
          |  }
          |
          |  override def $$owned: Boolean = {
          |    return owned
          |  }
          |
          |  override def $$owned_=(b: Boolean): MutableMarker = {
          |    owned = b
          |    return this
          |  }
          |
          |  override def $$clone: MutableMarker = {
          |    // not expecting users to want to clone realizations of this abstract class
          |    return this
          |  }
          |
          |  override def beforeEach(): Unit = {
          |    BeforeEntrypoint()
          |  }
          |
          |  override def afterEach(): Unit = {
          |    AfterEntrypoint()
          |  }
          |}"""
    return ResourceUtil.createResource(Util.pathAppend(projectDirectories.testUtilDir, ISZ(names.packagePath, s"${className2Use}.scala")), ret, T)
  }
}
