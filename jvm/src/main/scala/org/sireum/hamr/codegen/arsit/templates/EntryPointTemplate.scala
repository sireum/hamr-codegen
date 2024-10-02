// #Sireum
package org.sireum.hamr.arsit.templates

import org.sireum._
import org.sireum.hamr.arsit.EntryPoints

@datatype class EntryPointTemplate(parameters: ISZ[ST],
                                   localVars: ISZ[ST],

                                   defaultActivateBody: ST,

                                   defaultInitialiseBody: ST,
                                   defaultTestInitialiseBody: ST,

                                   defaultComputeBody: ST,
                                   defaultTestComputeBody: ST,

                                   // Note that the following entry points are not emitted
                                   defaultDeactivateBody: ST,
                                   defaultFinaliseBody: ST,
                                   defaultRecoverBody: ST) {

  @pure def generateCustom(blocks: ISZ[String],
                           activateBody: Option[String],
                           initialiseBody: Option[String],
                           testInitialiseBody: Option[String],
                           computeBody: Option[String],
                           testComputeBody: Option[String],

                           // Note that the following entry points are not emitted
                           deactivateBody: Option[String],
                           finaliseBody: Option[String],
                           recoverBody: Option[String]): ST = {
    @strictpure def toST(o: Option[String], default: ST): ST = if (o.nonEmpty) st"${o.get}" else default

    return generate(
      blocks = localVars ++ blocks.map((s: String) => st"$s"),
      activateBody = toST(activateBody, defaultActivateBody),
      initialiseBody = toST(initialiseBody, defaultInitialiseBody),
      testInitialiseBody = toST(testInitialiseBody, defaultTestInitialiseBody),
      computeBody = toST(computeBody, defaultComputeBody),
      testComputeBody = toST(testComputeBody, defaultTestComputeBody),
      deactivateBody = toST(deactivateBody, defaultDeactivateBody),
      finaliseBody = toST(finaliseBody, defaultFinaliseBody),
      recoverBody = toST(recoverBody, defaultRecoverBody)
    )
  }

  @pure def generateCustomST(blocks: ISZ[ST],
                             activateBody: Option[ST],
                             initialiseBody: Option[ST],
                             testInitialiseBody: Option[ST],
                             computeBody: Option[ST],
                             testComputeBody: Option[ST],

                             // Note that the following entry points are not emitted
                             deactivateBody: Option[ST],
                             finaliseBody: Option[ST],
                             recoverBody: Option[ST]): ST = {
    return generate(
      blocks = localVars ++ blocks,
      activateBody = if (activateBody.nonEmpty) activateBody.get else defaultActivateBody,
      initialiseBody = if (initialiseBody.nonEmpty) initialiseBody.get else defaultInitialiseBody,
      testInitialiseBody = if (testInitialiseBody.nonEmpty) testInitialiseBody.get else defaultTestInitialiseBody,
      computeBody = if (computeBody.nonEmpty) computeBody.get else defaultComputeBody,
      testComputeBody = if (testComputeBody.nonEmpty) testComputeBody.get else defaultTestComputeBody,
      deactivateBody = if (deactivateBody.nonEmpty) deactivateBody.get else defaultDeactivateBody,
      finaliseBody = if (finaliseBody.nonEmpty) finaliseBody.get else defaultFinaliseBody,
      recoverBody = if (recoverBody.nonEmpty) recoverBody.get else defaultRecoverBody
    )
  }

  @pure def generateDefault(): ST = {
    return generate(localVars, defaultActivateBody, defaultInitialiseBody, defaultTestInitialiseBody,
      defaultComputeBody, defaultTestComputeBody, defaultDeactivateBody, defaultFinaliseBody, defaultRecoverBody)
  }

  @pure def generate(blocks: ISZ[ST],

                     activateBody: ST,

                     initialiseBody: ST,
                     testInitialiseBody: ST,

                     computeBody: ST,
                     testComputeBody: ST,

                     // Note that the following entry points are not emitted
                     deactivateBody: ST,
                     finaliseBody: ST,
                     recoverBody: ST): ST = {
    val entryPoint: ST =
      st"""@datatype class EntryPoints(
          |  ${(parameters, ",\n")}) extends Bridge.EntryPoints {
          |  ${(blocks, "\n\n")}
          |
          |  def ${EntryPoints.initialise.name}(): Unit = {
          |    $initialiseBody
          |  }
          |
          |  def ${EntryPoints.compute.name}(): Unit = {
          |    $computeBody
          |  }
          |
          |  def ${EntryPoints.finalise.name}(): Unit = {
          |    $finaliseBody
          |  }
          |
          |  override
          |  def ${EntryPoints.testInitialise.name}(): Unit = {
          |    $testInitialiseBody
          |  }
          |
          |  override
          |  def ${EntryPoints.testCompute.name}(): Unit = {
          |    $testComputeBody
          |  }
          |}"""
    return entryPoint
  }
}
