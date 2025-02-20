// #Sireum
package org.sireum.hamr.codegen.microkit.connections

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MemoryRegion
import org.sireum.hamr.codegen.microkit.util.Util.TAB

@sig trait ConnectionStore {
  //def connectionPath: ISZ[String]

  def systemContributions: SystemContributions

  def typeApiContributions: ISZ[TypeApiContributions]

  def senderName: ISZ[String]

  def senderContributions: Option[UberConnectionContributions]

  // component path -> contributions
  def receiverContributions: Map[ISZ[String], UberConnectionContributions]
}

@datatype class DefaultConnectionStore (val systemContributions: SystemContributions,
                                        val typeApiContributions: ISZ[TypeApiContributions],
                                        val senderName: ISZ[String],
                                        val senderContributions: Option[UberConnectionContributions],
                                        val receiverContributions: Map[ISZ[String], UberConnectionContributions]) extends ConnectionStore

@sig trait SystemContributions {
  def sharedMemoryRegionContributions: ISZ[MemoryRegion]

  def channelContributions: ISZ[ST]
}

@datatype class DefaultSystemContributions(val sharedMemoryRegionContributions: ISZ[MemoryRegion],
                                           val channelContributions: ISZ[ST]) extends SystemContributions

@sig trait TypeApiContributions {
  def aadlType: AadlType

  def simpleFilename: String

  @pure def objectName: String = {
    return s"$$(TOP_DIR)/build/$simpleFilename.o"
  }

  @pure def headerFilename: String = {
    return s"$simpleFilename.h"
  }

  @pure def implementationFilename: String = {
    return s"$simpleFilename.c"
  }

  @pure def buildEntry: ST = {
    return (
      st"""$objectName: $$(TOP_DIR)/${MicrokitTypeUtil.cTypesDir}/src/$implementationFilename Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_INCLUDE)
        """)
  }

  def header: ST

  def implementation: ST
}

@datatype class DefaultTypeApiContributions(val aadlType: AadlType,
                                            val simpleFilename: String,
                                            @hidden val header: ST,
                                            @hidden val implementation: ST) extends  TypeApiContributions

@sig trait GlobalVarContribution {
  def typ: String
  def varName: String

  def pretty: ST = {
    return st"$typ $varName;"
  }
}
@datatype class PortVaddr(val typ: String,
                          val varName: String) extends GlobalVarContribution

@datatype class QueueVaddr(val typ: String,
                          val varName: String) extends GlobalVarContribution

@datatype class VMRamVaddr (val typ: String,
                            val varName:String) extends GlobalVarContribution

@sig trait ConnectionContributions {

  def portName: ISZ[String]

  def portPriority: Option[Z]

  def aadlType: AadlType

  def queueSize: Z

  def sharedMemoryMapping: ISZ[MemoryRegion]
}

@sig trait cLangConnectionContributions {

  def cHeaderImportContributions: ISZ[String]

  def cImplementationImportContributions: ISZ[String]

  def cUserMethodSignatures: ISZ[ST]

  def cUserMethodDefaultImpls: ISZ[ST]

  def cDefineContributions: ISZ[ST]

  def cGlobalVarContributions: ISZ[GlobalVarContribution]

  def cApiMethodSigs: ISZ[ST]

  def cApiMethods: ISZ[ST]

  def cInitContributions: ISZ[ST]

  def cComputeContributions: ISZ[ST]
}

@datatype class cConnectionContributions (val cHeaderImportContributions: ISZ[String],
                                          val cApiMethodSigs: ISZ[ST],

                                          val cUserMethodSignatures: ISZ[ST],
                                          val cGlobalVarContributions: ISZ[GlobalVarContribution],
                                          val cDefineContributions: ISZ[ST],
                                          val cApiMethods: ISZ[ST],

                                          val cImplementationImportContributions: ISZ[String],

                                          val cInitContributions: ISZ[ST],
                                          val cComputeContributions: ISZ[ST],

                                          val cUserMethodDefaultImpls: ISZ[ST]) extends cLangConnectionContributions

@sig trait rustLangConnectionContributions {
  def rustExternApis: ISZ[ST]

  def rustUnsafeExternApisWrappers: ISZ[ST]
}

@datatype class rustConnectionsContributions(val rustExternApis: ISZ[ST],
                                             val rustUnsafeExternApisWrappers: ISZ[ST]) extends rustLangConnectionContributions

@datatype class UberConnectionContributions(val portName: ISZ[String],
                                            val portPriority: Option[Z],

                                            val aadlType: AadlType,
                                            val queueSize: Z,

                                            val sharedMemoryMapping: ISZ[MemoryRegion],

                                            val cContributions: cLangConnectionContributions,

                                            val rustContributions: rustLangConnectionContributions) extends ConnectionContributions