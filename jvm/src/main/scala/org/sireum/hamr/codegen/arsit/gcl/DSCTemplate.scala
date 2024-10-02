// #Sireum
package org.sireum.hamr.arsit.gcl

import org.sireum._
import org.sireum.hamr.arsit.gcl.GumboXGenUtil.GGParam
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.util.NameUtil.NameProvider

object DSCTemplate {

  val dscContainerSuffix: String = "DSC_TestVector"

  val recordUnsatPreObjectName: String = "DSC_RecordUnsatPre"

  val tq: String = "\"\"\""
  val tqq: String = "\"\\\"\\\"\\\"\""

  def genTestVectorContainerClass(packageName: String,
                                  imports: ISZ[String],
                                  containers: ISZ[ST]): ST = {

    val _imports: ISZ[ST] = for (i <- imports) yield st"import ${i}"

    val ret: ST =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |${(_imports, "\n")}
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |// containers for the pre and post state values of ports and state variables
          |
          |${(containers, "\n\n")}
          |"""
    return ret
  }

  def genNextProfileMethod(methodName: String,
                           profileType: String,
                           dscContainerType: String,
                           nextCalls: ISZ[ST],
                           actuals: ISZ[ST]): ST = {
    val ret =
      st"""def $methodName(profile: $profileType): Option[$dscContainerType] = {
          |  try {
          |    ${(nextCalls, "\n")}
          |
          |    return Some(${dscContainerType}(${(actuals, ",")}))
          |  } catch {
          |    case e: AssertionError =>
          |     // SlangCheck was unable to satisfy a datatype's filter
          |     return None()
          |  }
          |}"""
    return ret
  }

  def genProfileRunner(profileTestMethodName: String,
                       profileType: String,

                       unitTestPrefix: String,
                       testCBMethodNameVector: String,

                       nameProvider: NameProvider,
                       dscContainerType: String,

                       nextProfileMethodName: String,
                       testVectorPrettyPrints: ISZ[ST],
                       testComputeMethodCall: String): ST = {

    val testName = st"""Profile \"$${profile.name}\": ${unitTestPrefix}_$$i"""
    val escapedTestName = st"""Profile \\\"$${profile.name}\\\": ${unitTestPrefix}_$$i"""

    val replay: Option[ST] =
      if (testVectorPrettyPrints.nonEmpty)
        Some(
          st"""if (verbose) {
              |  val tq = $tqq
              |  println(st${tq}Replay Unit Test:
              |              |  test("Replay: $$escapedTestName") {
              |              |    val json = st$${tq}$${json}$${tq}.render
              |              |    val testVector = ${jsonMethod(F, nameProvider, dscContainerType)}(json).left
              |              |    assert (${testCBMethodNameVector}(testVector) == ${nameProvider.basePackage}.GumboXUtil.GumboXResult.$$results)
              |              |  }$tq.render)
              |}
              |""")
      else None()

    val ret =
      st"""def ${profileTestMethodName}(profile: $profileType): Unit = {
          |  for (i <- 0 until profile.numTests) {
          |    val testName = s"$testName"
          |    val escapedTestName = s"$escapedTestName"
          |
          |    this.registerTest(testName) {
          |      var retry: B = T
          |
          |      var j: Z = 0
          |      while (j < profile.numTestVectorGenRetries && retry) {
          |        $nextProfileMethodName(profile) match {
          |          case Some(o) =>
          |
          |            if (verbose && j > 0) {
          |              println(s"Retry $$j:")
          |            }
          |
          |            val results = $testComputeMethodCall
          |
          |            val json = ${jsonMethod(T, nameProvider, dscContainerType)}(o, T)
          |            updateReport("${profileTestMethodName}", results.name, testName, j, Some(json))
          |
          |            $replay
          |            results match {
          |              case GumboXResult.Pre_Condition_Unsat =>
          |              case GumboXResult.Post_Condition_Fail =>
          |                fail ("Post condition did not hold")
          |                retry = F
          |              case GumboXResult.Post_Condition_Pass =>
          |                if (verbose) {
          |                  println ("Success!")
          |                }
          |                retry = F
          |            }
          |          case _ =>
          |            updateReport("${profileTestMethodName}", "SlangCheck RTS", testName, j, None())
          |        }
          |        j = j + 1
          |      }
          |
          |      if (retry) {
          |        if (failOnUnsatPreconditions) {
          |          fail ("Unable to satisfy precondition")
          |        } else if (verbose) {
          |          cprintln(T, "Unable to satisfy precondition")
          |        }
          |      }
          |    }
          |  }
          |}"""
    return ret
  }

  def genInitializeScalaTests(profileTestName: String, profileType: String,
                              unitTestPrefix: String, testInitializeMethodName: String): ST = {
    val testName = st"""Profile \"$${profile.name}\": ${unitTestPrefix}_$$i"""
    val ret =
      st"""def $profileTestName(profile: $profileType): Unit = {
          |  for (i <- 0 until profile.numTests) {
          |    val testName = s"$testName"
          |    this.registerTest(testName) {
          |      val results = $testInitializeMethodName()
          |      updateReport("testInitialiseCB", results.name, testName, 0, None())
          |
          |      results match {
          |        case GumboXResult.Pre_Condition_Unsat =>
          |          halt("Infeasible as initialize entry points cannot contain assume clauses and cannot access incoming ports or state variables")
          |        case GumboXResult.Post_Condition_Fail =>
          |          fail ("Post condition did not hold")
          |        case GumboXResult.Post_Condition_Pass =>
          |          if (verbose) {
          |            println ("Success!")
          |          }
          |      }
          |    }
          |  }
          |}"""
    return ret
  }

  def jsonMethod(from: B, componentNames: NameProvider, dscContainerType: String): ST = {
    val sergenName = st"${(ops.ISZOps(componentNames.packageNameI).drop(1), "")}$dscContainerType"
    return st"${componentNames.basePackage}.JSON.${if (from) "from" else "to"}${sergenName}"
  }

  def genNextDSCMethod(ranLibDecls: ISZ[ST],
                       dscContainerType: String,
                       ranLibInvocations: ISZ[ST],
                       params: ISZ[GGParam]): ST = {
    val ret =
      st"""${(ranLibDecls, "\n")}
          |
          |override def next(): ${dscContainerType} = {
          |  ${(ranLibInvocations, "\n")}
          |  return ${dscContainerType}(
          |    ${(for (p <- GumboXGenUtil.sortParam(params)) yield p.name, ", ")}
          |  )
          |}"""
    return ret
  }

  def dscRunnerClass(packageName: String,
                     basePackage: String,
                     testRunners: ISZ[ST]): ST = {

    val dscRunnerContent =
      st"""// #Sireum
          |
          |package $packageName
          |
          |import org.sireum._
          |import ${basePackage}.GumboXUtil.GumboXResult
          |import ${basePackage}.RandomLib
          |import org.sireum.Random.Gen64
          |import org.sireum.Random.Impl.Xoshiro256
          |
          |${CommentTemplate.doNotEditComment_scala}
          |
          |// Distribute SlangCheck test runners
          |
          |${(testRunners, "\n\n")}
          |"""

    return dscRunnerContent
  }

  def dscTestRunner(nameProvider: NameProvider,
                    runnerClassName: String,
                    dscContainerType: String,
                    slangTestHarnessName: String,

                    nextMethod: ST,

                    methodCall: String
                   ): ST = {
    val ret =
      st"""@record class $runnerClassName
          |  extends Random.Gen.TestRunner[$dscContainerType]
          |  with $slangTestHarnessName {
          |
          |  val verbose: B = F
          |
          |  $nextMethod
          |
          |  override def toCompactJson(o: $dscContainerType): String = {
          |    return ${jsonMethod(T, nameProvider, dscContainerType)}(o, T)
          |  }
          |
          |  override def fromJson(json: String): $dscContainerType = {
          |    ${jsonMethod(F, nameProvider, dscContainerType)}(json) match {
          |      case Either.Left(o) => return o
          |      case Either.Right(msg) => halt(msg.string)
          |    }
          |  }
          |
          |  override def test(o: $dscContainerType): B = {
          |    BeforeEntrypoint()
          |    val r: B = $methodCall match {
          |      case GumboXResult.Pre_Condition_Unsat =>
          |        ${nameProvider.basePackage}.$recordUnsatPreObjectName.report(${jsonMethod(T, nameProvider, dscContainerType)}(o, T))
          |        T
          |      case GumboXResult.Post_Condition_Fail => F
          |      case GumboXResult.Post_Condition_Pass => T
          |    }
          |    AfterEntrypoint()
          |    return r
          |  }
          |}"""
    return ret
  }

  def dscRecordUnsatPreArtifacts(basePackageName: String): ST = {
    val ret =
      st"""// #Sireum
          |
          |package ${basePackageName}
          |
          |import org.sireum._
          |
          |${CommentTemplate.safeToEditComment_scala}
          |
          |object $recordUnsatPreObjectName {
          |
          |  /** report will be called when a test vector generated by Distributed Slang Check does
          |    * not satisfy an entry point's assume/require clauses.  The test vector could, e.g.,
          |    * be written out to a file as DSC does for the passing and failing vectors
          |    *
          |    * @param testVector the JSON serialized test vector
          |    */
          |  def report(testVector: String): Unit = {
          |
          |  }
          |
          |}
          |"""
    return ret
  }
}
