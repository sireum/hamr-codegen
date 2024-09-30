// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._

@datatype class MakefileContainer(val resourceSuffix: String,
                                  val relativePath: Option[String],
                                  val hasHeader: B,
                                  val hasUserContent: B) {

  @strictpure def cHeaderFilename: String = s"$resourceSuffix.h"

  @strictpure def cImplFilename: String = s"$resourceSuffix.c"

  @strictpure def cUserImplFilename: String = s"${resourceSuffix}_user.c"

  @strictpure def objName: String = s"$resourceSuffix.o"

  @strictpure def userObjName: String = s"${resourceSuffix}_user.o"

  @strictpure def monObjName: String = s"${resourceSuffix}_MON.o"

  @strictpure def elfName: String = s"$resourceSuffix.elf"

  @strictpure def monElfName: String = s"${resourceSuffix}_MON.elf"

  @strictpure def monImplFilename: String = s"${resourceSuffix}_MON.c"



  @pure def getElfNames: ISZ[String] = {
    var ret: ISZ[String] = ISZ(elfName)
    if (hasUserContent) {
      ret = ret :+ monElfName
    }
    return ret
  }

  @pure def getObjNames: ISZ[String] = {
    var ret: ISZ[String] = ISZ(objName)
    if (hasUserContent) {
      ret = ret :+ userObjName
      ret = ret :+ monObjName
    }
    return ret
  }

  @strictpure def relativePathSrcDir: String = if (relativePath.nonEmpty) s"${relativePath.get}/src" else ""

  @strictpure def relativePathIncludeDir: String = if (relativePath.nonEmpty) s"${relativePath.get}/include" else ""

  @pure def OBJSEntry: ST = {
    var ret = st"${ops.StringOps(resourceSuffix).toUpper}_OBJS := $$(PRINTF_OBJS) $objName"
    if (hasUserContent) {
      ret = st"""${ops.StringOps(s"${resourceSuffix}_MON").toUpper}_OBJS := $$(PRINTF_OBJS) $monObjName
                |$ret"""
    }
    return ret
  }

  @pure def buildEntry: ST = {
    val TAB: String = "\t"
    // FIXME spilt output into include and src directories
    val header: Option[String] = if (hasHeader) Some(s" -I$${TOP}/$relativePathIncludeDir") else None()
    val userContributions: Option[ST] =
      if (hasUserContent)
        Some(
          st"""# monitor
              |$monObjName: $${TOP}/$relativePathSrcDir/$monImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include$header
              |
              |# user code
              |$userObjName: $${TOP}/$relativePathSrcDir/$cUserImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include$header
              |""")
      else None()
    val ret =
      st"""$userContributions
          |$objName: $${TOP}/$relativePathSrcDir/$cImplFilename Makefile
          |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$${TOP}/include$header"""
    return ret
  }

  @pure def elfEntry: ST = {
    val TAB: String = "\t"
    if (hasUserContent) {
      val ret =
        st"""$monElfName: $$(PRINTF_OBJS) $monObjName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@
            |
            |$elfName: $$(PRINTF_OBJS) $userObjName $objName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@"""
      return ret
    } else {
      val ret =
        st"""$elfName: $$(PRINTF_OBJS) $objName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@"""
      return ret
    }
  }
}


