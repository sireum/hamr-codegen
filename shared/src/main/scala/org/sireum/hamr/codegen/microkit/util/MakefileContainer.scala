// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.microkit.types.TypeUtil

@datatype class MakefileContainer(val resourceSuffix: String,
                                  val relativePath: Option[String],
                                  val hasHeader: B,
                                  val isVM: B,
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

  @strictpure def vmArchive: String = s"${resourceSuffix}.a"

  @pure def getElfNames: ISZ[String] = {
    var ret: ISZ[String] = ISZ(elfName)
    if (hasUserContent || isVM) {
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

  @strictpure def relativePathDir: String = if (relativePath.nonEmpty) s"${relativePath.get}" else ""

  @strictpure def relativePathSrcDir: String = if (relativePath.nonEmpty) s"${relativePathDir}/src" else ""

  @strictpure def relativePathIncludeDir: String = if (relativePath.nonEmpty) s"${relativePathDir}/include" else ""

  @strictpure def relativePathVmBoardDir: String = if (relativePath.nonEmpty) s"${relativePathDir}/board" else ""

  @pure def OBJSEntry: ST = {
    var ret = st"${ops.StringOps(resourceSuffix).toUpper}_OBJS := $$(${TypeUtil.make_TYPE_OBJS}) $objName"
    if (hasUserContent) {
      ret = st"""${ops.StringOps(s"${resourceSuffix}_MON").toUpper}_OBJS := $$(${TypeUtil.make_TYPE_OBJS}) $monObjName
                |$ret"""
    }
    return ret
  }

  @pure def buildEntry: ST = {
    val TAB: String = "\t"
    // FIXME spilt output into include and src directories
    val header: Option[String] = if (hasHeader) Some(s" -I$$(TOP_DIR)/$relativePathIncludeDir") else None()
      if (hasUserContent) {
        return(
          st"""# monitor
              |$monObjName: $$(TOP_DIR)/$relativePathSrcDir/$monImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_INCLUDE)$header
              |
              |# user code
              |$userObjName: $$(TOP_DIR)/$relativePathSrcDir/$cUserImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_INCLUDE)/$header
              |
              |$objName: $$(TOP_DIR)/$relativePathSrcDir/$cImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_INCLUDE)$header""")
      } else if (isVM) {
        return(
          st"""# monitor
              |$monObjName: $$(TOP_DIR)/$relativePathSrcDir/$monImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ $$(TOP_INCLUDE)$header
              |
              |# $vmArchive contains a VM
              |.PHONY: $vmArchive
              |$vmArchive:
              |ifeq (, $$(wildcard $$(TOP_DIR)/${relativePathDir}/board/$$(MICROKIT_BOARD)/Makefile))
              |${TAB}$$(error Didn't find: $$(TOP_DIR)/$relativePathDir/board/$$(MICROKIT_BOARD)/Makefile);
              |endif
              |${TAB}mkdir -p $$(TOP_DIR)/$relativePathDir/build
              |${TAB}cp $$(TOP_DIR)/$relativePathDir/board/$${MICROKIT_BOARD}/Makefile $$(TOP_DIR)/$relativePathDir/build
              |${TAB}make -C $$(TOP_DIR)/$relativePathDir/build
              |""")
      }
      else {
        return (
          st"""
              |$objName: $$(TOP_DIR)/$relativePathSrcDir/$cImplFilename Makefile
              |${TAB}$$(CC) -c $$(CFLAGS) $$< -o $$@ -I$$(TOP_INCLUDE)$header""")
      }
  }

  @pure def elfEntry: ST = {
    val TAB: String = "\t"
    if (hasUserContent) {
      val ret =
        st"""$monElfName: $monObjName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@
            |
            |$elfName: $$(${Util.make_UTIL_OBJS}) $$(${TypeUtil.make_TYPE_OBJS}) $userObjName $objName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@"""
      return ret
    } else if (isVM) {
      val ret =
        st"""$monElfName: $monObjName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@
            |
            |$elfName: $$(${TypeUtil.make_TYPE_OBJS}) $vmArchive
            |${TAB}$$(LD) $$(LDFLAGS)  --start-group -lmicrokit -Tmicrokit.ld $$(TYPE_OBJS) $vmArchive --end-group -o $$@"""
      return ret
    }
    else {
      val ret =
        st"""$elfName: $$(${Util.make_UTIL_OBJS}) $$(${TypeUtil.make_TYPE_OBJS}) $objName
            |${TAB}$$(LD) $$(LDFLAGS) $$^ $$(LIBS) -o $$@"""
      return ret
    }
  }
}


