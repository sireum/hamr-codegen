// #Sireum

package org.sireum.hamr.codegen.act.templates

import org.sireum._
import org.sireum.hamr.codegen.act.ast
import org.sireum.hamr.codegen.act.util._
import org.sireum.hamr.codegen.act.vm.VM_Template
import org.sireum.hamr.codegen.common.symbols.{AadlDataPort, AadlPort}
import org.sireum.hamr.codegen.common.templates.{CommentTemplate, StackFrameTemplate}
import org.sireum.hamr.codegen.common.{CommonUtil, StringUtil}
import org.sireum.hamr.ir

object StringTemplate {
  val SEM_DISPATCH: String = Util.brand("dispatch_sem")

  val SB_VERIFY: String = Util.cbrand("VERIFY")

  val MON_READ_ACCESS: String = Util.cbrand("MONITOR_READ_ACCESS")
  val MON_WRITE_ACCESS: String = Util.cbrand("MONITOR_WRITE_ACCESS")

  val SeqNumName: String = "seqNum"
  val SeqNumType: String = s"${SeqNumName}_t"

  val bt: String = "\\"

  def cHeaderFile(filename: String,
                  includes: ISZ[String],
                  entries: ISZ[ST]): ST = {
    val macroName = StringUtil.macroize(filename)

    val _includes: Option[ST] = if (includes.nonEmpty) {
      Some(st"${(includes.map((m: String) => st"#include ${m}"), "\n")}\n")
    } else {
      None()
    }

    val _entries: Option[ST] = if (entries.nonEmpty) {
      Some(st"${(entries, "\n\n")}")
    } else {
      None()
    }

    val ret: ST =
      st"""${CommentTemplate.doNotEditComment_c}
          |
          |#ifndef ${macroName}
          |#define ${macroName}
          |
          |${_includes}
          |${_entries}
          |
          |#endif // ${macroName}
          |"""
    return ret
  }

  def tbTypeHeaderFile(filename: String,
                       includes: Option[ST],
                       defs: ISZ[ST],
                       preventBadging: B): ST = {

    val badges: ST = if (preventBadging) {
      st""
    } else {
      st"""
          |#define $MON_READ_ACCESS 111
          |#define $MON_WRITE_ACCESS 222"""
    }

    val macroName = StringUtil.macroize(filename)

    val body =
      st"""#ifndef ${macroName}
          |#define ${macroName}
          |
          |#include <stdio.h>
          |#include <stdbool.h>
          |#include <stdint.h>
          |${includes}
          |
          |#ifndef ${SB_VERIFY}
          |#include <stddef.h>
          |#endif // ${SB_VERIFY}
          |
          |#define __${Util.cbrand("OS")}_CAMKES__${badges}
          |
          |#ifndef ${SB_VERIFY}
          |#define MUTEXOP(OP)${bt}
          |if((OP) != 0) {${bt}
          |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);${bt}
          |  *((int*)0)=0xdeadbeef;${bt}
          |}
          |#else
          |#define MUTEXOP(OP) OP
          |#endif // ${SB_VERIFY}
          |#ifndef ${SB_VERIFY}
          |#define CALLBACKOP(OP)${bt}
          |if((OP) != 0) {${bt}
          |  fprintf(stderr,"Operation " #OP " failed in %s at %d.\n",__FILE__,__LINE__);${bt}
          |  *((int*)0)=0xdeadbeef;${bt}
          |}
          |#else
          |#define CALLBACKOP(OP) OP
          |#endif // ${SB_VERIFY}
          |
          |${(defs, "\n\n")}
          |
          |#endif // ${macroName}
          |"""
    return body
  }

  def tbMissingType(): ST = {
    return (
      st"""// placeholder for unspecified types in the AADL model
          |typedef bool ${Util.MISSING_AADL_TYPE};""")
  }

  val receivedDataVar: String = "receivedData"

  def tbMonReadWrite(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                     preventBadging: B): ST = {
    val read: ST =
      st"""*m = contents;
          |return ${receivedDataVar};"""

    val mon_read: ST = if (preventBadging) {
      read
    } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else {
          |  ${read}}
          |}"""
    }

    val write: ST =
      st"""${receivedDataVar} = true;
          |contents = *m;
          |monsig_emit();
          |return ${receivedDataVar};"""
    val mon_write: ST = if (preventBadging) {
      write
    } else {
      st"""bool mon_write(const $typeName * m) {
          |  if (mon_get_sender_id() != $MON_WRITE_ACCESS)  {
          |    return false;
          |  } else {
          |    ${write}
          |  }
          |}"""
    }

    val senderSig: ST = if (preventBadging) {
      st""
    } else {
      st"""
                                                               |int mon_get_sender_id(void);"""
    }
    val r: ST =
      st"""#include ${typeHeaderFilename}
          |#include <${monitorTypeHeaderFilename}.h>
          |
          |${senderSig}int monsig_emit(void);
          |
          |static $typeName contents;
          |bool ${receivedDataVar} = false;
          |
          |bool mon_is_empty() {
          |  return !${receivedDataVar};
          |}
          |
          |bool mon_read($typeName * m) {
          |  ${mon_read}
          |}
          |
          |bool mon_write(const $typeName * m) {
          |  ${mon_write}
          |}"""

    return r
  }

  def tbEnqueueDequeue(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                       preventBadging: B): ST = {

    val mon_dequeue: ST = if (preventBadging) {
      st""
    } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else """
    }

    val mon_enqueue: ST = if (preventBadging) {
      st""
    } else {
      st"""if (mon_get_sender_id() != $MON_WRITE_ACCESS) {
          |    return false;
          |} else """
    }

    val r: ST =
      st"""#ifndef $SB_VERIFY
          |#include <stdio.h>
          |#endif // $SB_VERIFY
          |
          |#include ${typeHeaderFilename}
          |#include <${monitorTypeHeaderFilename}.h>
          |
          |int mon_get_sender_id(void);
          |int monsig_emit(void);
          |
          |${typeName} contents[${dim}];
          |static uint32_t front = 0;
          |static uint32_t length = 0;
          |
          |static bool is_full(void) {
          |  return length == ${dim};
          |}
          |
          |bool mon_is_empty(void) {
          |  return length == 0;
          |}
          |
          |bool mon_dequeue(${typeName} * m) {
          |  ${mon_dequeue}if (mon_is_empty()) {
          |    return false;
          |  } else {
          |    *m = contents[front];
          |    front = (front + 1) % ${dim};
          |    length--;
          |    return true;
          |  }
          |}
          |
          |bool mon_enqueue(const ${typeName} * m) {
          |  ${mon_enqueue}if (is_full()) {
          |    return false;
          |  } else {
          |    contents[(front + length) % ${dim}] = *m;
          |    length++;
          |    monsig_emit();
          |    return true;
          |  }
          |}
          |"""
    return r
  }

  def tbRaiseGetEvents(queueSize: Z, monitorTypeHeaderFilename: String,
                       preventBadging: B): ST = {
    var r: ST =
      st"""#include <camkes.h>
          |#include <stdio.h>
          |#include <string.h>
          |
          |int32_t num_events = 0;
          |
          |static inline void ignore_result(long long int unused_result) { (void) unused_result; }
          |
          |// Send interfaces
          |bool mon_send_enqueue(void) {
          |  int do_emit = 0;
          |  ignore_result(m_lock());
          |  if (num_events < ${queueSize}) {
          |    num_events++;
          |    do_emit = 1;
          |  }
          |  ignore_result(m_unlock());
          |  if (do_emit) {
          |    monsig_emit();
          |  }
          |  return true;
          |}
          |
          |// Receive interfaces
          |bool mon_receive_is_empty(void) {
          |  return num_events == 0;
          |}
          |
          |bool mon_receive_dequeue(void) {
          |  ignore_result(m_lock());
          |  bool ret = false;
          |  if(num_events > 0){
          |    num_events--;
          |    ret = true;
          |  }
          |  ignore_result(m_unlock());
          |  return ret;
          |}
          |"""

    return r
  }

  def tbEnqueueDequeueIhor(typeName: String, dim: Z, monitorTypeHeaderFilename: String, typeHeaderFilename: String,
                           preventBadging: B): ST = {

    val mon_dequeue: ST = if (preventBadging) {
      st""
    } else {
      st"""if (mon_get_sender_id() != $MON_READ_ACCESS) {
          |  return false;
          |} else """
    }

    val mon_enqueue: ST = if (preventBadging) {
      st""
    } else {
      st"""if (mon_get_sender_id() != $MON_WRITE_ACCESS) {
          |    return false;
          |} else """
    }

    val r: ST =
      st"""#ifndef $SB_VERIFY
          |#include <stdio.h>
          |#endif // $SB_VERIFY
          |#include <camkes.h>
          |#include ${typeHeaderFilename}
          |#include "<${monitorTypeHeaderFilename}.h>"
          |
          |struct queue {
          |    int head;
          |    int tail;
          |    int len;
          |    ${typeName} elt[${dim}];
          |} q = {.head=0, .tail=0, .len=0};
          |
          |static bool is_full(void) {
          |  return q.len == ${dim};
          |}
          |
          |bool mon_is_empty(void) {
          |  return q.len == 0;
          |}
          |
          |bool mon_receive_dequeue(${typeName} * m) {
          |  ${mon_dequeue}if (mon_is_empty()) {
          |    return false;
          |  } else {
          |    m_lock();
          |    *m = q.elt[q.tail];
          |    q.tail = (q.tail + 1) % ${dim};
          |    q.len--;
          |    m_unlock();
          |    return true;
          |  }
          |}
          |
          |bool mon_send_enqueue(const ${typeName} * m) {
          |  ${mon_enqueue}if (is_full()) {
          |    return false;
          |  } else {
          |    m_lock();
          |    q.elt[q.head] = *m;
          |    q.head = (q.head + 1) % ${dim};
          |    q.len++;
          |    m_unlock();
          |    monsig_emit();    
          |    return true;
          |  }
          |}
          |"""
    return r
  }

  def seqNumHeader(): ST = {
    return (
      st"""#ifndef _SEQNUM_H_
          |#define _SEQNUM_H_
          |
          |// Typedef for seqNum to make it easy to change the type. Keep these consistent!
          |typedef uintmax_t seqNum_t;
          |#define SEQNUM_MAX UINTMAX_MAX
          |#define PRIseqNum PRIuMAX
          |
          |// DIRTY_SEQ_NUM is used to mark a sampling port message as dirty while it is
          |// being writen. DIRTY_SEQ_NUM is not a valid sequence number. Valid sequence
          |// numbers are from 0 to DIRTY_SEQ_NUM-1 is never a valid sequence number.
          |static const seqNum_t DIRTY_SEQ_NUM = SEQNUM_MAX;
          |
          |#endif""")
  }

  def sbSamplingPortGlobalVar(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    val globalVarName = s"${Util.brand(aadlPort.identifier)}_seqNum"
    return st"$globalVarName"
  }

  def sbSamplingPortGlobalVarDecl(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    return st"${StringTemplate.SeqNumType} ${sbSamplingPortGlobalVar(spi, aadlPort)};"
  }

  def sbSamplingPortGetterInterface(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    val methodNamePrefix = Util.brand(aadlPort.identifier)

    val ret: ST = st"bool ${methodNamePrefix}_read(${spi.sel4TypeName} * value);"

    return ret
  }

  def sbSamplingPortSetterInterface(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    val methodNamePrefix = Util.brand(aadlPort.identifier)

    val ret: ST = st"bool ${methodNamePrefix}_write(const ${spi.sel4TypeName} * value);"

    return ret
  }

  def sbSamplingPortInterface(spi: SamplingPortInterface, f: ir.FeatureEnd): ST = {
    assert(f.category == ir.FeatureCategory.DataPort)

    val portName = CommonUtil.getLastName(f.identifier)
    val methodNamePrefix = Util.brand(portName)

    val ret: ST = f.direction match {
      case ir.Direction.In => st"bool ${methodNamePrefix}_read(${spi.sel4TypeName} * value);"
      case ir.Direction.Out => st"bool ${methodNamePrefix}_write(const ${spi.sel4TypeName} * value);"
      case _ => halt(s"Unexpected direction ${f.direction}")
    }
    return ret
  }

  def sbSamplingPortGetterImplementation(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    val sharedDataVarName = Util.brand(aadlPort.identifier)
    val globalVarName = sbSamplingPortGlobalVar(spi, aadlPort)

    val isEmptyMethodName = s"${sharedDataVarName}_is_empty"

    val ret: ST =
      st"""/*****************************************************************
          | * ${isEmptyMethodName}:
          | *
          | * Helper method to determine if the data infrastructure port has
          | * received data
          | *
          | ****************************************************************/
          |bool ${isEmptyMethodName}() {
          |  return is_empty_${spi.name}(${sharedDataVarName});
          |}
          |
          |bool ${sharedDataVarName}_read(${spi.sel4TypeName} * value) {
          |  ${StringTemplate.SeqNumType} new_seqNum;
          |  if ( read_${spi.name}(${sharedDataVarName}, value, &new_seqNum) ) {
          |    ${globalVarName} = new_seqNum;
          |    return true;
          |  } else {
          |    return false;
          |  }
          |}"""
    return ret
  }

  def sbSamplingPortInitialise(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    val featureName = aadlPort.identifier
    val sharedDataVarName = Util.brand(aadlPort.identifier)
    val globalVarName = sbSamplingPortGlobalVar(spi, aadlPort)

    val initMethodName = s"init_${spi.name}"

    return (
      st"""// initialise data structure for data port ${featureName}
          |${initMethodName}(${sharedDataVarName}, &${globalVarName});""")
  }

  def sbSamplingPortSetterImplementation(spi: SamplingPortInterface, aadlPort: AadlDataPort): ST = {
    val sharedDataVarName = Util.brand(aadlPort.identifier)
    val globalVarName = sbSamplingPortGlobalVar(spi, aadlPort)

    return (
      st"""bool ${sharedDataVarName}_write(const ${spi.sel4TypeName} * value) {
          |  return write_${spi.name}(${sharedDataVarName}, value, &${globalVarName});
          |}""")
  }

  def sbAccessRestrictionEntry(componentName: String, varName: String, permission: String): ast.Configuration = {
    return ast.GenericConfiguration(st"""${componentName}.${varName}_access = "${permission}";""".render, ISZ())
  }

  def configurationPriority(name: String, priority: Z): ast.Configuration = {
    return ast.GenericConfiguration(s"${name}.priority = ${priority};", ISZ())
  }

  def configurationControlStackSize(name: String, size: Z): ast.Configuration = {
    return ast.GenericConfiguration(s"${name}._control_stack_size = ${size};", ISZ())
  }

  def configurationStackSize(name: String, size: Z): ast.Configuration = {
    return ast.GenericConfiguration(s"${name}._stack_size = ${size};", ISZ())
  }

  val SEM_WAIT: String = Util.brand("dispatch_sem_wait")
  val SEM_POST: String = Util.brand("dispatch_sem_post")

  def componentPreInitGlueCode(preInits: ISZ[ST], isSeL4: B, fileUri: String): Option[ST] = {

    val preInit: Option[ST] = if (preInits.nonEmpty) {
      val methodName = "pre_init"

      val declNewStackFrame: Option[ST] = if (isSeL4) {
        val d = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)
        Some(
          st"""${d};
              |""")
      } else {
        None()
      }

      Some(
        st"""
            |void ${methodName}(void) {
            |  ${declNewStackFrame}
            |  ${(preInits, "\n\n")}
            |}""")
    } else {
      None()
    }
    return preInit
  }

  def componentPostInitGlueCode(postInits: ISZ[ST], isSeL4: B, fileUri: String): Option[ST] = {
    val methodName = "post_init"

    val declNewStackFrame: Option[ST] = if (isSeL4) {
      val d = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)
      Some(
        st"""${d};
            |""")
    } else {
      None()
    }

    val postInit: Option[ST] = if (postInits.nonEmpty) {
      Some(
        st"""
            |void ${methodName}(void) {
            |  ${declNewStackFrame}
            |  ${(postInits, "\n\n")}
            |}""")
    } else {
      None()
    }
    return postInit
  }

  def runMethod(locals: ISZ[ST],
                initStmts: ISZ[ST],
                preLoopStmts: ISZ[ST],
                loopStartStmts: ISZ[ST],
                loopBodyStmts: ISZ[ST],
                loopEndStmts: ISZ[ST],
                postLoopStmts: ISZ[ST],
                containsFFIs: B,
                isSeL4: B,
                fileUri: String): ST = {

    def flatten(i: ISZ[ST]): Option[ST] = {
      return if (i.nonEmpty) Some(st"""${(i, "\n")}""") else None()
    }

    val methodName = "run"

    val declNewStackFrame: Option[ST] = if (isSeL4) {
      val d = StackFrameTemplate.DeclNewStackFrame(F, fileUri, "", methodName, 0)
      Some(
        st"""${d};
            |""")
    } else {
      None()
    }

    val ret: ST =
      st"""/************************************************************************
          | * int run(void)
          | * Main active thread function.
          | ************************************************************************/
          |int ${methodName}(void) {
          |  ${declNewStackFrame}
          |  ${flatten(locals)}
          |  ${flatten(initStmts)}
          |  ${flatten(preLoopStmts)}
          |  for(;;) {
          |    ${flatten(loopStartStmts)}
          |    ${flatten(loopBodyStmts)}
          |    ${flatten(loopEndStmts)}
          |  }
          |  ${flatten(postLoopStmts)}
          |  return 0;
          |}"""

    if (containsFFIs) {
      return (
        st"""#ifndef ${CakeMLTemplate.PREPROCESSOR_CAKEML_ASSEMBLIES_PRESENT}
            |${ret}
            |#endif""")
    } else {
      return ret
    }
  }

  def componentTypeImpl(componentHeaderFilename: String,
                        includes: ISZ[ST],

                        blocks: ISZ[ST],
                        preInit: Option[ST],
                        postInit: Option[ST],

                        runMethod: ST): ST = {

    val filteredIncludes: Set[String] = Set.empty[String] ++ includes.map((s: ST) => s.render)

    val ret: ST =
      st"""${CommentTemplate.doNotEditComment_c}
          |
          |#include <${componentHeaderFilename}>
          |${(filteredIncludes.elements, "\n")}
          |#include <string.h>
          |#include <camkes.h>
          |
          |${(blocks, "\n\n")}
          |${preInit}
          |${postInit}
          |
          |${runMethod}
          |"""
    return ret
  }

  def componentInitializeEntryPoint(componentName: String, methodName: String): (ST, ST) = {
    val init: String = Util.brand(s"entrypoint_${componentName}_initializer")
    val ret: ST =
      st"""/************************************************************************
          | *  ${init}:
          | *
          | * This is the function invoked by an active thread dispatcher to
          | * call to a user-defined entrypoint function.  It sets up the dispatch
          | * context for the user-defined entrypoint, then calls it.
          | *
          | ************************************************************************/
          |void ${init}(const int64_t * in_arg) {
          |  ${methodName}((int64_t *) in_arg);
          |}"""
    val dummy = Util.brand("dummy")
    val runEntry: ST =
      st"""{
          |  int64_t ${dummy};
          |  ${init}(&${dummy});
          |}"""
    return (ret, runEntry)
  }

  def cEventNotificationHandler(handlerName: String, regCallback: String, featureName: String): ST = {
    val ret: ST =
      st"""/************************************************************************
          | * ${handlerName}:
          | * Invoked by: seL4 notification callback
          | *
          | * This is the function invoked by an seL4 notification callback to 
          | * dispatch the component due to the arrival of an event on port
          | * ${featureName}
          | *
          | ************************************************************************/
          |static void ${handlerName}(void * unused) {
          |  MUTEXOP(${SEM_POST}())
          |  CALLBACKOP(${regCallback}(${handlerName}, NULL));
          |}"""
    return ret
  }

  def cRegCallback(handlerName: String, regCallback: String, aadlPort: AadlPort): ST = {
    val portType = aadlPort.feature.category.string
    val featureName = aadlPort.identifier
    val ret: ST =
      st"""// register callback for ${portType} port ${featureName}
          |CALLBACKOP(${regCallback}(${handlerName}, NULL));"""
    return ret
  }

  def samplingPortFreezeMethodName(aadlPort: AadlPort): String = {
    return Util.brand(s"freeze_event_port_${aadlPort.identifier}")
  }

  def samplingPortHeader(s: SamplingPortInterface): ST = {
    val macroName = StringUtil.toUpperCase(s"${s.name}_h")

    val ret =
      st"""#ifndef ${macroName}
#define ${macroName}

#include ${Util.getSbTypeHeaderFilenameForIncludes()}
#include <seqNum.h>

// Sampling port message with bool data
typedef struct ${s.name} {

  // The sampling port message data.
  //
  ${s.sel4TypeName} data;
  
  // Sequence number incremented by the writer every time the sampling port is
  // written. Read by the receiver to detect dropped messages and incoherent
  // message reads.  An incoherent message is one that is formed of parts of
  // more than one message.  An incoherent message can occur when writing
  // happens during read. If the component runs long enough, this counter
  // will wrap back to zero.  This causes no problems unless the receiver is
  // delayed for the wrap time. In that case the receiver may not detect
  // dropped or incoherent message. But if the receiver is delayed for that
  // long the system is probably in a very bad state. Also see DIRTY_SEQ_NUM
  // above.
  //
  // TODO: Currently using ggc builtin _Atomic. Would like to use c11 std, but
  // have not figured out how to do this int the seL4 cmake build environment.
  _Atomic seqNum_t seqNum;  

} ${s.structName};

void init_${s.name}(${s.structName} *port, seqNum_t *seqNum);

bool write_${s.name}(${s.structName} *port, const ${s.sel4TypeName} *data, seqNum_t *seqNum);

bool read_${s.name}(${s.structName} *port, ${s.sel4TypeName} *data, seqNum_t *seqNum);

bool is_empty_${s.name}(${s.structName} *port);

#endif
"""
    return ret
  }

  def samplingPortImpl(s: SamplingPortInterface): ST = {

    val ret =
      st"""

#include <${s.name}.h>

void init_${s.name}(${s.structName} *port, seqNum_t *seqNum) {
  *seqNum = 0; // First message sequence number will be 1.
  port->seqNum = DIRTY_SEQ_NUM;
}

// Write message to a sampling port (data type: int)
//
// Returns true when successful. Otherwise returns false. Currently there is no
// way to fail and true is always returned. But this may change in the
// future. seqNum is incremented when a message is successfully sent. seqNum
// should not be modified otherwise.
//
// TODO: Encapsulate this better. seqNum state should be maintained internally. Possible solutions:
//
//    - Allow write to have read access to dataport. Then seqNum is simply in the data port.
//
//    - Create a wrapper struct.
//
// TODO: Currently using ggc builtin __atomic_thread_fence(__ATOMIC_RELEASE).
// Would like to use c11 std, but have not figured out how to do this int the
// seL4 cmake build environment.
bool write_${s.name}(${s.structName} *port, const ${s.sel4TypeName} *data, seqNum_t *seqNum) {
  // Mark the message dirty BEFORE we start writing.
  port->seqNum = DIRTY_SEQ_NUM;
  // Release memory fence - ensure write above to seqNum happens BEFORE reading data
  __atomic_thread_fence(__ATOMIC_RELEASE);
  // Write the data
  port->data = *data;
  // Increment the sequence number. We are the only writer of seqNum, so
  // increment does not have to be atomic.
  *seqNum = (*seqNum + 1) % DIRTY_SEQ_NUM;
  port->seqNum = *seqNum;
  // Release memory fence - ensure write above to seqNum happens BEFORE continuing
  __atomic_thread_fence(__ATOMIC_RELEASE);
  // Can't fail for now.
  return true;
}

// Read a message from a sampling port (data type: int)
//
// Return true upon successful read. Data is updated with the read
// message. The sequence number of the message is also returned. The message,
// might be tha same previously read. The sequences number can be used to
// detect rereading the same message or dropped messages.
//
// Return false if we fail to read a message. For now the only way to fail is
// when we detect the possibility of a write during read. In this case data
// may be incoherent and should not be used. Sequence number is set to
// DIRTY_SEQ_NUM;
//
// TODO: Currently using ggc builtin __atomic_thread_fence(__ATOMIC_ACQUIRE).
// Would like to use c11 std, but have not figured out how to do this int the
// seL4 cmake build environment.
bool read_${s.name}(${s.structName} *port, ${s.sel4TypeName} *data, seqNum_t *seqNum) {
  seqNum_t newSeqNum = port->seqNum;
  // Acquire memory fence - Read seqNum BEFORE reading data
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
  *data = port->data;
  // Acquire memory fence - Read data BEFORE reading seqNum again 
  //atomic_thread_fence(memory_order_acquire);
  __atomic_thread_fence(__ATOMIC_ACQUIRE);
  // The following logic will NOT catch case where the writer wrapped
  // sequence numbers since our last read. For this to happen, this reader
  // would have to be delayed for the entire time to wrap. 
  if (newSeqNum != DIRTY_SEQ_NUM && newSeqNum == port->seqNum) {
    // Message data is good.  Write did not occur during read. 
    *seqNum = newSeqNum;
    return true;
  } else {
    // Writer may have updated data while we were reading. Do not use possibly incoherent data.
    *seqNum = DIRTY_SEQ_NUM;
    return false;
  }
}

bool is_empty_${s.name}(${s.structName} *port) {
  return port->seqNum == DIRTY_SEQ_NUM;
}
"""
    return ret
  }

  def ifEsleHelper(options: ISZ[(ST, ST)], optElse: Option[ST]): ST = {
    val first: Option[(ST, ST)] = if (options.size > 0) {
      Some(options(0))
    } else {
      None()
    }
    val rest: ISZ[(ST, ST)] = if (options.size > 1) {
      org.sireum.ops.ISZOps(options).drop(1)
    } else {
      ISZ()
    }
    return ifElseST(first, rest, optElse)
  }

  def ifElseST(ifbranch: Option[(ST, ST)], elsifs: ISZ[(ST, ST)], els: Option[ST]): ST = {

    var body = st""

    if (ifbranch.nonEmpty) {
      body =
        st"""if(${ifbranch.get._1}) {
            |  ${ifbranch.get._2}
            |} """
    }

    if (elsifs.nonEmpty) {
      val ei = elsifs.map((x: (ST, ST)) =>
        st"""else if(${x._1}) {
            |  ${x._2}
            |} """)
      body = st"""${body}${ei}"""
    }

    if (els.nonEmpty) {
      if (ifbranch.nonEmpty) {
        body =
          st"""${body}else {
              |  ${els.get}
              |}"""
      } else {
        body = els.get
      }
    }

    return body
  }

  def consumes(c: ast.Consumes): ST = {
    val maybe: String = if (c.optional) "maybe " else ""
    return st"${maybe}consumes ${c.typ} ${c.name};"
  }

  def dataport(d: ast.Dataport): ST = {
    val maybe: String = if (d.optional) "maybe " else ""
    return st"${maybe}dataport ${d.typ} ${d.name};"
  }

  def emits(e: ast.Emits): ST = {
    return st"emits ${e.typ} ${e.name};"
  }

  def provides(p: ast.Provides): ST = {
    return st"provides ${p.typ} ${p.name};"
  }

  def uses(u: ast.Uses): ST = {
    val maybe: String = if (u.optional) "maybe " else ""
    return st"${maybe}uses ${u.typ} ${u.name};"
  }

  def postGenInstructionsMessage(camkesProjDirectory: String,
                                 cakeMLAssemblyLocations: ISZ[String],
                                 runCamkesScript: String,
                                 hasVM: B): ST = {

    var hints: ISZ[ST] = ISZ()
    var cakeMLLocs: Option[ST] = None()

    if (cakeMLAssemblyLocations.nonEmpty) {
      hints = hints :+
        st"""Pass '-o "-D${CakeMLTemplate.PREPROCESSOR_CAKEML_ASSEMBLIES_PRESENT}=ON"' when the CAkeML assemblies are in place."""

      cakeMLLocs = Some(
        st"""
            |Location of CakeML assemblies:
            |  ${(cakeMLAssemblyLocations, "\n")}""")
    }

    if (hasVM) {
      hints = hints :+ st"""Your project contains VMs. The pre-configured rootfs will be used unless '-o "-D${VM_Template.BUILD_CROSSVM}=ON"' is passed"""
    }

    val ret: ST =
      st"""CAmkES Instructions:
          |--------------------
          |  CAmkES Project Directory: ${camkesProjDirectory}
          |  ${cakeMLLocs}
          |
          |  Execute the following to simulate the system via QEMU. Pass '-h' to see the available options.
          |
          |    ${runCamkesScript} -s
          |
          |    ${(hints, "\n\n")}"""

    return ret
  }
}
