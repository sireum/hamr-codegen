// #Sireum
package org.sireum.hamr.codegen.microkit.connections

import org.sireum._
import org.sireum.hamr.codegen.common.types.AadlType
import org.sireum.hamr.codegen.microkit.types.MicrokitTypeUtil
import org.sireum.hamr.codegen.microkit.util.MemoryRegion
import org.sireum.hamr.codegen.microkit.util.Util.TAB

@sig trait ConnectionStore {

  def systemContributions: SystemContributions

  def typeApiContributions: ISZ[TypeApiContributions]

  def senderName: ISZ[String]

  def codeContributions: Map[ISZ[String], UberConnectionContributions]
}

@datatype class DefaultConnectionStore (val systemContributions: SystemContributions,
                                        val typeApiContributions: ISZ[TypeApiContributions],
                                        val senderName: ISZ[String],
                                        val codeContributions: Map[ISZ[String], UberConnectionContributions]) extends ConnectionStore

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

  def cBridge_EntrypointMethodSignatures: ISZ[ST]

  def cUser_MethodDefaultImpls: ISZ[ST]

  def cBridge_GlobalVarContributions: ISZ[GlobalVarContribution]

  def cPortApiMethodSigs: ISZ[ST]

  def cBridge_PortApiMethods: ISZ[ST]

  def cBridge_InitContributions: ISZ[ST]

  def cBridge_ComputeContributions: ISZ[ST]
}

object cConnectionContributions {
  @pure def empty: cConnectionContributions = {
    return cConnectionContributions(ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ(), ISZ())
  }
}

@datatype class cConnectionContributions (val cPortApiMethodSigs: ISZ[ST],

                                          val cBridge_EntrypointMethodSignatures: ISZ[ST],
                                          val cBridge_GlobalVarContributions: ISZ[GlobalVarContribution],
                                          val cBridge_PortApiMethods: ISZ[ST],
                                          val cBridge_InitContributions: ISZ[ST],
                                          val cBridge_ComputeContributions: ISZ[ST],

                                          val cUser_MethodDefaultImpls: ISZ[ST]) extends cLangConnectionContributions {
  @pure def combine(other: cLangConnectionContributions): cConnectionContributions = {
    return cConnectionContributions(
      cPortApiMethodSigs = this.cPortApiMethodSigs ++ other.cPortApiMethodSigs,
      cBridge_EntrypointMethodSignatures = this.cBridge_EntrypointMethodSignatures ++ other.cBridge_EntrypointMethodSignatures,
      cBridge_GlobalVarContributions = this.cBridge_GlobalVarContributions ++ other.cBridge_GlobalVarContributions,
      cBridge_PortApiMethods = this.cBridge_PortApiMethods ++ other.cBridge_PortApiMethods,
      cBridge_InitContributions = this.cBridge_InitContributions ++ other.cBridge_InitContributions,
      cBridge_ComputeContributions = this.cBridge_ComputeContributions ++ other.cBridge_ComputeContributions,
      cUser_MethodDefaultImpls = this.cUser_MethodDefaultImpls ++ other.cUser_MethodDefaultImpls
    )
  }
}

@datatype class UberConnectionContributions(val portName: ISZ[String],
                                            val portPriority: Option[Z],

                                            val aadlType: AadlType,
                                            val queueSize: Z,

                                            val sharedMemoryMapping: ISZ[MemoryRegion],

                                            val cContributions: cLangConnectionContributions) extends ConnectionContributions
