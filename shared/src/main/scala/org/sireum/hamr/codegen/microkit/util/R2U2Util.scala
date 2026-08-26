// #Sireum
package org.sireum.hamr.codegen.microkit.util

import org.sireum._
import org.sireum.hamr.codegen.common.CommonUtil.Store
import org.sireum.hamr.codegen.common.containers.Resource
import org.sireum.hamr.codegen.common.templates.CommentTemplate
import org.sireum.hamr.codegen.common.util.ResourceUtil

object R2U2Util {

  val platformIncludeRelativePath: String = s"${MicrokitUtil.utilDir}/r2u2/include"

  val runtimeRelativeSources: ISZ[String] = ISZ(
    "engines/booleanizer.c",
    "engines/engines.c",
    "engines/mltl.c",
    "instructions/booleanizer.c",
    "instructions/mltl.c",
    "internals/process_binary.c",
    "lib.c",
    "memory/monitor.c",
    "memory/shared_connection_queue.c")

  // Generate the R2U2 platform headers required by the native C runtime.
  @pure def platformResources(outputDir: String): ISZ[Resource] = {
    val includePath: String = s"$outputDir/$platformIncludeRelativePath"
    return ISZ[Resource](
      ResourceUtil.createResource(s"$includePath/stdio.h", r2u2stdioh, T),
      ResourceUtil.createResource(s"$includePath/string.h", r2u2stringh, T),
      ResourceUtil.createResource(s"$includePath/math.h", r2u2mathh, T))
  }

  // Generate the shared C R2U2 build rules.
  @pure def globalBuildEntry(store: Store): ST = {
    val TAB: String = "\t"
    val r2u2CliVersion: String = MicrokitUtil.getMicrokitVersions(store).get("r2u2").get
    val r2u2Tag: String = MicrokitUtil.getMicrokitVersions(store).get("r2u2-c").get
    return st"""# R2U2 C runtime
        |R2U2_REPOSITORY ?= https://github.com/R2U2/r2u2.git
        |R2U2_TAG ?= $r2u2Tag
        |R2U2_DIR ?= $$(TOP_DIR)/dep/r2u2-$$(R2U2_TAG)
        |R2U2_SRC_DIR := $$(R2U2_DIR)/monitors/c/src
        |R2U2_PLATFORM_INCLUDE := $$(TOP_DIR)/$platformIncludeRelativePath
        |R2U2_PLATFORM_HEADERS := $$(addprefix $$(R2U2_PLATFORM_INCLUDE)/,stdio.h string.h math.h)
        |
        |R2U2_CPPFLAGS := -I$$(R2U2_PLATFORM_INCLUDE) -I$$(R2U2_SRC_DIR)
        |R2U2_RUNTIME_REL_SRCS := \
        |${TAB}${(runtimeRelativeSources, s" \\\n${TAB}")}
        |R2U2_RUNTIME_SRCS := $$(addprefix $$(R2U2_SRC_DIR)/,$$(R2U2_RUNTIME_REL_SRCS))
        |
        |.PHONY: r2u2_cli
        |r2u2_cli:
        |${TAB}@echo "Checking/Updating r2u2_cli from crates.io..."
        |${TAB}cargo +stable install r2u2_cli --version $r2u2CliVersion
        |
        |$$(R2U2_SRC_DIR)/r2u2.h:
        |${TAB}@if [ ! -d "$$(R2U2_DIR)/.git" ]; then \
        |${TAB}${TAB}mkdir -p "$$(dir $$(R2U2_DIR))"; \
        |${TAB}${TAB}git clone --depth 1 --single-branch --branch "$$(R2U2_TAG)" --filter=blob:none --no-checkout "$$(R2U2_REPOSITORY)" "$$(R2U2_DIR)"; \
        |${TAB}fi
        |${TAB}@if [ ! -f "$$@" ]; then \
        |${TAB}${TAB}git -C "$$(R2U2_DIR)" sparse-checkout init --no-cone && \
        |${TAB}${TAB}git -C "$$(R2U2_DIR)" sparse-checkout set '/monitors/c/src/' && \
        |${TAB}${TAB}git -C "$$(R2U2_DIR)" checkout --detach HEAD; \
        |${TAB}fi
        |
        |$$(R2U2_RUNTIME_SRCS): | $$(R2U2_SRC_DIR)/r2u2.h
        |
        |clean::
        |${TAB}rm -rf r2u2"""
  }

  // Generate the C R2U2 build rules for a component.
  @pure def buildEntry(mk: MakefileContainer): ST = {
    assert(mk.requiresR2U2 && mk.relativePath.nonEmpty)
    val TAB: String = "\t"
    val prefix: String = s"${ops.StringOps(mk.resourceSuffix).toUpper}_R2U2"
    return st"""# R2U2 monitor for ${mk.resourceSuffix}
        |${prefix}_BUILD_DIR := r2u2/${mk.resourceSuffix}
        |${prefix}_OBJ_DIR := $$(${prefix}_BUILD_DIR)/obj
        |${prefix}_BOUNDS := $$(${prefix}_BUILD_DIR)/bounds.h
        |${prefix}_SPEC_BIN := $$(${prefix}_BUILD_DIR)/spec.bin
        |${prefix}_SPEC_C := $$(${prefix}_BUILD_DIR)/spec_bin.c
        |${prefix}_CPPFLAGS := $$(R2U2_CPPFLAGS) -include $$(${prefix}_BOUNDS)
        |${prefix}_RUNTIME_OBJS := $$(addprefix $$(${prefix}_OBJ_DIR)/,$$(R2U2_RUNTIME_REL_SRCS:.c=.o))
        |${prefix}_OBJS := $$(${prefix}_RUNTIME_OBJS) \
        |${TAB}$$(${prefix}_OBJ_DIR)/r2u2_monitor.o \
        |${TAB}$$(${prefix}_OBJ_DIR)/spec_bin.o
        |
        |$$(${prefix}_SPEC_BIN): $$(TOP_DIR)/${mk.relativePathSrcDir}/spec.c2po $$(TOP_DIR)/${mk.relativePathSrcDir}/spec.map Makefile | r2u2_cli
        |${TAB}mkdir -p "$$(${prefix}_BUILD_DIR)"
        |${TAB}sed '/^--/d' "$$(TOP_DIR)/${mk.relativePathSrcDir}/spec.map" > "$$(${prefix}_BUILD_DIR)/temp.map"
        |${TAB}r2u2_cli compile --disable-aux -o "$$(${prefix}_BUILD_DIR)" -b "$$(${prefix}_BOUNDS)" "$$(TOP_DIR)/${mk.relativePathSrcDir}/spec.c2po" "$$(${prefix}_BUILD_DIR)/temp.map" && \
        |${TAB}${TAB}xxd -i -n r2u2_spec_bin "$$(${prefix}_SPEC_BIN)" "$$(${prefix}_SPEC_C)"
        |
        |$$(${prefix}_OBJ_DIR)/spec_bin.o: $$(${prefix}_SPEC_BIN) Makefile
        |${TAB}mkdir -p "$$(@D)"
        |${TAB}$$(CC) $$(CFLAGS) -c "$$(${prefix}_SPEC_C)" -o "$$@"
        |
        |$$(${prefix}_OBJ_DIR)/r2u2_monitor.o: $$(TOP_DIR)/${mk.relativePathSrcDir}/r2u2_monitor.c $$(TOP_DIR)/${mk.relativePathIncludeDir}/r2u2_monitor.h $$(${prefix}_SPEC_BIN) $$(R2U2_SRC_DIR)/r2u2.h $$(R2U2_PLATFORM_HEADERS) Makefile
        |${TAB}mkdir -p "$$(@D)"
        |${TAB}$$(CC) $$(${prefix}_CPPFLAGS) $$(CFLAGS) -Wno-\#warnings -c "$$<" -o "$$@" $$(TOP_INCLUDE) -I$$(TOP_DIR)/${mk.relativePathIncludeDir}
        |
        |$$(${prefix}_OBJ_DIR)/%.o: $$(R2U2_SRC_DIR)/%.c $$(${prefix}_SPEC_BIN) $$(R2U2_PLATFORM_HEADERS) Makefile
        |${TAB}mkdir -p "$$(@D)"
        |${TAB}$$(CC) $$(${prefix}_CPPFLAGS) $$(CFLAGS) -Wno-\#warnings -Wno-logical-op-parentheses -Wno-unused-variable -c "$$<" -o "$$@"
        |
        |${mk.objName}: $$(TOP_DIR)/${mk.relativePathIncludeDir}/r2u2_monitor.h"""
  }

  val r2u2stdioh: ST =
    st"""#ifndef R2U2_STDIO_H
        |#define R2U2_STDIO_H
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// R2U2 declares an optional output file, so FILE must exist even though HAMR
        |// delivers verdicts through out_func and leaves monitor.out_file NULL.
        |typedef struct r2u2_file FILE;
        |
        |// R2U2 compiles optional file logging, but this stub is not reached because
        |// HAMR leaves monitor.out_file NULL and uses the verdict callback instead.
        |static inline int fprintf(FILE *stream, const char *format, ...)
        |{
        |    (void)stream;
        |    (void)format;
        |    return 0;
        |}
        |
        |// R2U2 compiles a string signal loader, but HAMR does not load string signals.
        |static inline int sscanf(const char *str, const char *format, ...)
        |{
        |    (void)str;
        |    (void)format;
        |    __builtin_trap();
        |}
        |
        |#endif // R2U2_STDIO_H
        |"""

  val r2u2stringh: ST =
    st"""#ifndef R2U2_STRING_H
        |#define R2U2_STRING_H
        |
        |#include <stddef.h>
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// R2U2 uses these routines while loading and initializing a specification.
        |// Their implementations are supplied by HAMR's generated util.c.
        |void *memcpy(void *restrict dest, const void *restrict src, size_t n);
        |void *memset(void *dest, int c, size_t n);
        |
        |// R2U2 compiles a string signal loader, but HAMR does not load string signals.
        |static inline int strcmp(const char *lhs, const char *rhs)
        |{
        |    (void)lhs;
        |    (void)rhs;
        |    __builtin_trap();
        |}
        |
        |static inline char *strchr(const char *str, int c)
        |{
        |    (void)str;
        |    (void)c;
        |    __builtin_trap();
        |}
        |
        |#endif // R2U2_STRING_H
        |"""

  val r2u2mathh: ST =
    st"""#ifndef R2U2_MATH_H
        |#define R2U2_MATH_H
        |
        |${CommentTemplate.doNotEditComment_slash}
        |
        |// R2U2 compiles pow and sqrt instructions, but GUMBO cannot produce them.
        |// These stubs let the unmodified R2U2 runtime compile without linking libm.
        |static inline double pow(double base, double exponent)
        |{
        |    (void)base;
        |    (void)exponent;
        |    __builtin_trap();
        |}
        |
        |static inline double sqrt(double x)
        |{
        |    (void)x;
        |    __builtin_trap();
        |}
        |
        |#endif // R2U2_MATH_H
        |"""
}
