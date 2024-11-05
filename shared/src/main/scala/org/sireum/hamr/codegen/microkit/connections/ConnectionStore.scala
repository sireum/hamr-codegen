// #Sireum
package org.sireum.hamr.codegen.microkit.connections

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.util.SharedMemoryRegion
import org.sireum.hamr.codegen.microkit.util.Util.TAB

@sig trait ConnectionStore {
  //def connectionPath: ISZ[String]

  def systemContributions: SystemContributions

  def typeApiContributions: ISZ[TypeApiContributions]

  def senderName: ISZ[String]

  def senderContributions: ConnectionContributions

  // component path -> contributions
  def receiverContributions: Map[ISZ[String], ConnectionContributions]
}

@datatype class DefaultConnectionStore (//val connectionPath: ISZ[String],
                                        val systemContributions: SystemContributions,
                                        val typeApiContributions: ISZ[TypeApiContributions],
                                        val senderName: ISZ[String],
                                        val senderContributions: ConnectionContributions,
                                        val receiverContributions: Map[ISZ[String], ConnectionContributions]) extends ConnectionStore

@sig trait SystemContributions {
  def sharedMemoryRegionContributions: ISZ[SharedMemoryRegion]

  def channelContributions: ISZ[ST]
}

@datatype class DefaultSystemContributions(val sharedMemoryRegionContributions: ISZ[SharedMemoryRegion],
                                           val channelContributions: ISZ[ST]) extends SystemContributions

@sig trait TypeApiContributions {
  def aadlType: AadlType

  def simpleFilename: String

  @pure def objectName: String = {
    return s"$simpleFilename.o"
  }

  @pure def headerFilename: String = {
    return s"$simpleFilename.h"
  }

  @pure def implementationFilename: String = {
    return s"$simpleFilename.c"
  }

  @pure def buildEntry: ST = {
    return (
      st"""$objectName: $${TOP}/src/$implementationFilename Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include
        """)
  }

  def header: ST

  def implementation: ST
}

@datatype class DefaultTypeApiContributions(val aadlType: AadlType,
                                            val simpleFilename: String,
                                            val header: ST,
                                            val implementation: ST) extends  TypeApiContributions

@sig trait ConnectionContributions {

  def portName: ISZ[String]

  def portPriority: Option[Z]

  def headerImportContributions: ISZ[String]

  def implementationImportContributions: ISZ[String]

  def userMethodSignatures: ISZ[ST]

  def userMethodDefaultImpls: ISZ[ST]

  def defineContributions: ISZ[ST]

  def globalVarContributions: ISZ[(String, String)]

  def apiMethodSigs: ISZ[ST]

  def apiMethods: ISZ[ST]

  def initContributions: ISZ[ST]

  def computeContributions: ISZ[ST]

  def sharedMemoryMapping: ISZ[SharedMemoryRegion]
}

@datatype class DefaultConnectionContributions(val portName: ISZ[String],
                                               val portPriority: Option[Z],
                                               val headerImportContributions: ISZ[String],
                                               val implementationImportContributions: ISZ[String],
                                               val userMethodSignatures: ISZ[ST],
                                               val userMethodDefaultImpls: ISZ[ST],
                                               val defineContributions: ISZ[ST],
                                               val globalVarContributions: ISZ[(String, String)],
                                               val apiMethodSigs: ISZ[ST],
                                               val apiMethods: ISZ[ST],
                                               val initContributions: ISZ[ST],
                                               val computeContributions: ISZ[ST],
                                               val sharedMemoryMapping: ISZ[SharedMemoryRegion]) extends ConnectionContributions