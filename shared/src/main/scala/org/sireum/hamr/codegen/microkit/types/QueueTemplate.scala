// #Sireum

package org.sireum.hamr.codegen.microkit.types

import org.sireum._
import org.sireum.hamr.codegen.common.StringUtil
import org.sireum.hamr.codegen.microkit.util.Util.brand

object QueueTemplate {

  def getTypeQueueName(queueElementTypeName: String, queueSize: Z): String = {
    return brand(s"queue_${queueElementTypeName}_${queueSize}")
  }

  def getTypeQueueFileName(typeName: String, queueSize: Z): String = {
    return s"${getTypeQueueName(typeName, queueSize)}.h"
  }

  def getTypeQueueTypeName(typeName: String, queueSize: Z): String = {
    return s"${getTypeQueueName(typeName, queueSize)}_t"
  }

  def getTypeRecvQueueName(typeName: String, queueSize: Z): String = {
    return s"${getTypeQueueName(typeName, queueSize)}_Recv"
  }

  def getTypeRecvQueueTypeName(typeName: String, queueSize: Z): String = {
    return s"${getTypeRecvQueueName(typeName, queueSize)}_t"
  }

  def getQueueSizeMacroName(queueName: String): String = {
    return StringUtil.toUpperCase(s"${queueName}_SIZE")
  }

  def getQueueInitMethodName(queueElementTypeName: String,
                             queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_init"
  }

  def getQueueInitMethod(sharedMemoryVar: String,
                         queueElementTypeName: String,
                         queueSize: Z): ST = {
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)
    return st"""${getQueueInitMethodName(queueElementTypeName, queueSize)}(($queueTypeName *) $sharedMemoryVar);"""
  }

  def getQueueEnqueueMethodName(queueElementTypeName: String,
                                queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_enqueue"
  }

  def getQueueDequeueMethodName(queueElementTypeName: String,
                                queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_dequeue"
  }

  def getQueueIsEmptyMethodName(queueElementTypeName: String,
                                queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_is_empty"
  }

  ///// SENDER APIs

  def getClientPutMethodSig(portName: String,
                            queueElementTypeName: String,
                            isEventPort: B): ST = {
    val param: Option[ST] = if (isEventPort) None() else Some(st"const $queueElementTypeName *data")
    val methodName = st"put_${portName}"
    return st"""bool $methodName($param)"""
  }

  def getClientPutMethod(portName: String,
                         queueElementTypeName: String,
                         entries: ISZ[ST],
                         isEventPort: B): ST = {
    val methodSig = getClientPutMethodSig(portName, queueElementTypeName, isEventPort)
    return (
      st"""$methodSig {
          |  ${(entries, "\n\n")}
          |
          |  return true;
          |}""")
  }

  def getClientPutEntry(sharedMemoryVarName: String,
                        queueElementTypeName: String,
                        queueSize: Z,
                        isEventPort: B): ST = {
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)
    val methodName = getQueueEnqueueMethodName(queueElementTypeName, queueSize)
    val optEvent: Option[ST] = {
      if (isEventPort) Some(st"""${queueElementTypeName} eventPayload = 0; // always send 0 as the event payload
                                |${queueElementTypeName} *data = &eventPayload;""")
      else None()
    }
    return (
      st"""$optEvent
          |$methodName(($queueTypeName *) $sharedMemoryVarName, ($queueElementTypeName *) data);""")
  }

  def getClientEnqueueSharedVarName(portName: String,
                                    queueSize: Z): String = {
    return s"${portName}_queue_${queueSize}"
  }

  ///// RECEIVER APIs

  def getClientRecvQueueTypeName(ququeElementTypeName: String,
                                 queueSize: Z): String = {
    return getTypeRecvQueueTypeName(ququeElementTypeName, queueSize)
  }

  def getClientRecvQueueName(portName: String): String = {
    return s"${portName}_recv_queue"
  }

  def getQueueRecvInitMethodName(queueElementTypeName: String,
                                 queueSize: Z): String = {
    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    return s"${queueName}_Recv_init"
  }

  def getClientRecvInitMethodCall(portName: String,
                                  queueElementTypeName:String,
                                  queueSize: Z): ST = {
    val methodName = getQueueRecvInitMethodName(queueElementTypeName, queueSize)
    val recvQueueVar = getClientRecvQueueName(portName)
    val sharedMemVar = getClientEnqueueSharedVarName(portName, queueSize)
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)
    return st"""${methodName}(&$recvQueueVar, ($queueTypeName *) $sharedMemVar);"""
  }


  def getClientIsEmptyMethodName(portName: String): ST = {
    return st"${portName}_is_empty"
  }

  def getClientIsEmptyMethodSig(portName: String): ST = {
    return st"bool ${getClientIsEmptyMethodName(portName)}(void)"
  }

  def getClientIsEmptyMethod(portName: String,
                             queueElementTypeName: String,
                             queueSize: Z): ST = {
    val methodName = getQueueIsEmptyMethodName(queueElementTypeName, queueSize)
    val recvQueueMemVarName = getClientRecvQueueName(portName)

    return st"""${getClientIsEmptyMethodSig(portName)} {
               |  return ${methodName}(&$recvQueueMemVarName);
               |}"""
  }

  def getClientDequeueSharedVarName(portName: String,
                                    queueSize: Z): String = {
    return s"${portName}_queue_${queueSize}"
  }

  def getClientGetterMethodName(portName: String): ST = {
    return st"get_${portName}_poll"
  }

  def getClientGetterMethodPollSig(portName: String,
                                   queueElementTypeName: String,
                                   isEventPort: B): ST = {
    val methodName = getClientGetterMethodName(portName)
    val optEventData: Option[ST]=
      if (isEventPort) None()
      else Some(st", $queueElementTypeName *data")
    return st"bool $methodName(${TypeUtil.eventCounterTypename} *numDropped$optEventData)"
  }

  def getClientGetterMethodPoll(portName: String,
                                queueElementTypeName: String,
                                queueSize: Z,
                                isEventPort: B): ST = {
    val apiMethodName = getQueueDequeueMethodName(queueElementTypeName, queueSize)
    val methodSig = getClientGetterMethodPollSig(portName, queueElementTypeName, isEventPort)
    val recvQueueMemVarName = getClientRecvQueueName(portName)
    val queueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)
    val optEvent: Option[ST] =
      if (isEventPort) Some(
        st"""${queueElementTypeName} eventPortPayload;
            |${queueElementTypeName} *data = &eventPortPayload;""")
      else None()
    return (
      st"""$methodSig {
          |  $optEvent
          |  return $apiMethodName(($queueTypeName *) &$recvQueueMemVarName, numDropped, data);
          |}""")
  }


  def getClientDataGetterMethod(portName: String,
                                queueElementTypeName: String,
                                queueSize: Z): ST = {
    val apiMethodName = getQueueDequeueMethodName(queueElementTypeName, queueSize)
    val methodSig = getClientGetterMethodSig(portName, queueElementTypeName, F)
    val recvQueueMemVarName = getClientRecvQueueName(portName)
    val queueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)

    val lastName = s"last_${portName}_payload"
    return (
      st"""${queueElementTypeName} $lastName;
          |
          |$methodSig {
          |  ${TypeUtil.eventCounterTypename} numDropped;
          |  ${queueElementTypeName} fresh_data;
          |  bool isFresh = false;
          |  if ($apiMethodName(($queueTypeName *) &$recvQueueMemVarName, &numDropped, &fresh_data)) {
          |    $lastName = fresh_data;
          |    isFresh = true;
          |  }
          |  *data = $lastName;
          |  return isFresh;
          |}""")
  }

  def getClientGetterMethodSig(portName: String,
                               queueElementTypeName: String,
                               isEventPort: B): ST = {
    val optEventDataPort: Option[ST] =
      if (isEventPort) None()
      else Some(st"$queueElementTypeName *data")
    return st"bool get_${portName}($optEventDataPort)"
  }

  def getClientGetterMethod(portName: String,
                            queueElementTypeName: String,
                            isEventPort: B): ST = {
    val methodSig = getClientGetterMethodSig(portName, queueElementTypeName, isEventPort)
    val methodName = getClientGetterMethodName(portName)
    val optEventData: Option[ST] =
      if(isEventPort) None() else Some(st", data")
    return (
      st"""$methodSig {
          |  ${TypeUtil.eventCounterTypename} numDropped;
          |  return $methodName (&numDropped$optEventData);
          |}""")
  }

  def getClientEventHandlerMethodName(portName: String): ST = {
    return st"handle_$portName"
  }

  def getClientEventHandlerMethodSig(portName: String): ST = {
    return st"""void ${getClientEventHandlerMethodName(portName)}(void)"""
  }

  def getClientSporadicComputeContributions(portName: String): ST = {
    val userMethodName = s"handle_${portName}"
    val isEmpty = getClientIsEmptyMethodName(portName)
    return (
      st"""if (!$isEmpty()) {
          |  ${userMethodName}();
          |}""")
  }

  def getClientSporadicDefaultImplContributions(portName: String): ST = {
    return (
      st"""void handle_$portName(void) {
          |  // implement me
          |}""")
  }

  def header(queueElementTypeName: String,
             queueSize: Z): ST = {

    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)

    val recvQueueName = getTypeRecvQueueName(queueElementTypeName, queueSize)
    val recvQueueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)

    val queueSizeMacroName = getQueueSizeMacroName(queueName)

    val r =
      st"""/*
          | * Copyright 2017, Data61
          | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
          | * ABN 41 687 119 230.
          | *
          | * Copyright 2019 Adventium Labs
          | * Modifications made to original
          | *
          | * This software may be distributed and modified according to the terms of
          | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
          | * See "LICENSE_BSD2.txt" for details.
          | *
          | * @TAG(DATA61_Adventium_BSD)
          | */
          |
          |// Single sender multiple receiver Queue implementation for AADL
          |// Ports. Every receiver receives the sent data (ie broadcast). The queue
          |// operations are all non-blocking. The sender enqueue always succeeds. A
          |// receiver dequeue can fail and drop data if the sender writes while the
          |// receiver is reading. This situation is detected unless the sender gets
          |// ahead of a receiver by more than COUNTER_MAX. Since COUNTER_MAX is typically
          |// 2^64 (see ${TypeUtil.eventCounterFilename}), this is extremely unlikely. If it does happen the
          |// only adverse effect is that the receiver will not detect all dropped
          |// elements.
          |
          |#pragma once
          |
          |#include <${TypeUtil.eventCounterFilename}>
          |#include <${TypeUtil.aadlTypesFilename}>
          |#include <stdbool.h>
          |
          |// Queue size must be an integer factor of the size for ${TypeUtil.eventCounterTypename} (an unsigned
          |// integer type). Since we are using standard C unsigned integers for the
          |// counter, picking a queue size that is a power of 2 is a good choice. We
          |// could alternatively set the size of our counter to the largest possible
          |// multiple of queue size. But then we would need to do our own modulo
          |// operations on the counter rather than depending on c's unsigned integer
          |// operations.
          |//
          |// Note: One cell in the queue is always considered dirty. Its the next
          |// element to be written. Thus the queue can only contain
          |// ${queueSizeMacroName}-1 elements.
          |#define ${queueSizeMacroName} ${queueSize + 1}
          |
          |// This is the type of the seL4 dataport (shared memory) that is shared by the
          |// sender and all receivers. This type is referenced in the sender and receiver
          |// component definition files. The seL4 runtime creates an
          |// instance of this struct.
          |typedef struct ${queueName} {
          |  // Number of elements enqueued by the sender. The implementation depends
          |  // on C's standard module behaviour for unsigned integers. The counter never
          |  // overflows. It just wraps modulo the size of the counter type. The counter
          |  // is typically very large (see ${TypeUtil.eventCounterFilename}), so this should happen very
          |  // infrequently. Depending in C to initialize this to zero.
          |  _Atomic ${TypeUtil.eventCounterTypename} numSent;
          |
          |  // Queue of elements of type ${queueElementTypeName}
          |  // (see ${TypeUtil.typesFilename}) implemented as a ring buffer.
          |  // No initialization necessary.
          |  ${queueElementTypeName} elt[${queueSizeMacroName}];
          |
          |} ${queueTypeName};
          |
          |//------------------------------------------------------------------------------
          |// Sender API
          |//
          |// Could split this into separate header and source file since only sender
          |// code needs this.
          |
          |// Initialize the queue. Sender must call this exactly once before any calls to queue_enqueue();
          |void ${queueName}_init(${queueTypeName} *queue);
          |
          |// Enqueue data. This always succeeds and never blocks. Data is copied.
          |void ${queueName}_enqueue(
          |  ${queueTypeName} *queue,
          |  ${queueElementTypeName} *data);
          |
          |//------------------------------------------------------------------------------
          |// Receiver API
          |//
          |// Could split this into separate header and source file since only receiver
          |// code needs this.
          |
          |// Each receiver needs to create an instance of this.
          |typedef struct ${recvQueueName} {
          |  // Number of elements dequeued (or dropped) by a receiver. The implementation
          |  // depends on C's standard module behaviour for unsigned integers. The
          |  // counter never overflows. It just wraps modulo the size of the counter
          |  // type. The counter is typically very large (see counter.h), so this should
          |  // happen very infrequently.
          |  ${TypeUtil.eventCounterTypename} numRecv;
          |
          |  // Pointer to the actual queue. This is the seL4 dataport (shared memory)
          |  // that is shared by the sender and all receivers.
          |  ${queueTypeName} *queue;
          |
          |} ${recvQueueTypeName};
          |
          |// Each receiver must call this exactly once before any calls to other queue
          |// API functions.
          |void ${recvQueueName}_init(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${queueTypeName} *queue);
          |
          |// Dequeue data. Never blocks but can fail if the sender writes at same
          |// time.
          |
          |// When successful returns true. The dequeued data will be copied to
          |// *data. *numDropped will contain the number of elements that were dropped
          |// since the last call to queue_dequeue().
          |//
          |// When queue is empty, returns false and *numDropped is zero. *data is left in
          |// unspecified state.
          |//
          |// When dequeue fails due to possible write of data being read, returns false
          |// and *numDropped will be >= 1 specifying the number of elements that were
          |// dropped since the last call to ${queueName}_dequeue(). *data is left in
          |// unspecified state.
          |//
          |// If the sender ever gets ahead of a receiver by more than COUNTER_MAX,
          |// ${queueName}_dequeue will fail to count a multiple of COUNTER_MAX in
          |// numDropped. Since COUNTER_MAX is very large (typically on the order of 2^64,
          |// see ${TypeUtil.eventCounterFilename}), this is very unlikely.  If the sender is ever this far
          |// ahead of a receiver the system is probably in a very bad state.
          |bool ${queueName}_dequeue(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${TypeUtil.eventCounterTypename} *numDropped,
          |  ${queueElementTypeName} *data);
          |
          |// Is queue empty? If the queue is not empty, it will stay that way until the
          |// receiver dequeues all data. If the queue is empty you can make no
          |// assumptions about how long it will stay empty.
          |bool ${queueName}_is_empty(${recvQueueTypeName} *recvQueue);
          |"""
    return r
  }

  def implementation( //queueHeaderFilename: String,
                      queueElementTypeName: String,
                      queueSize: Z,

                      //counterTypeName: String
                    ): ST = {

    val queueName = getTypeQueueName(queueElementTypeName, queueSize)
    val queueTypeName = getTypeQueueTypeName(queueElementTypeName, queueSize)

    val queueHeaderFilename = getTypeQueueFileName(queueElementTypeName, queueSize)

    val recvQueueName = getTypeRecvQueueName(queueElementTypeName, queueSize)
    val recvQueueTypeName = getTypeRecvQueueTypeName(queueElementTypeName, queueSize)
    val queueSizeMacroName = getQueueSizeMacroName(queueName)

    val initMethodName = getQueueInitMethodName(queueElementTypeName, queueSize)
    val recvInitMethodName = getQueueRecvInitMethodName(queueElementTypeName, queueSize)
    val dequeueMethodName = getQueueDequeueMethodName(queueElementTypeName, queueSize)
    val enqueueMethodName = getQueueEnqueueMethodName(queueElementTypeName, queueSize)
    val isEmptyMethodName = getQueueIsEmptyMethodName(queueElementTypeName, queueSize)


    val r =
      st"""/*
          | * Copyright 2017, Data61
          | * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
          | * ABN 41 687 119 230.
          | *
          | * Copyright 2019 Adventium Labs
          | * Modifications made to original
          | *
          | * This software may be distributed and modified according to the terms of
          | * the BSD 2-Clause license. Note that NO WARRANTY is provided.
          | * See "LICENSE_BSD2.txt" for details.
          | *
          | * @TAG(DATA61_Adventium_BSD)
          | */
          |
          |#include <${queueHeaderFilename}>
          |#include <stdint.h>
          |#include <stddef.h>
          |
          |//------------------------------------------------------------------------------
          |// Sender API
          |//
          |// See ${queueHeaderFilename} for API documentation. Only implementation details are documented here.
          |
          |void ${initMethodName}(${queueTypeName} *queue) {
          |  // NOOP for now. C's struct initialization is sufficient.  If we ever do need
          |  // initialization logic, we may also need to synchronize with receiver
          |  // startup.
          |}
          |
          |void ${enqueueMethodName}(
          |  ${queueTypeName} *queue,
          |  ${queueElementTypeName} *data) {
          |
          |  // Simple ring with one dirty element that will be written next. Only one
          |  // writer, so no need for any synchronization.
          |  // elt[queue->numSent % ${queueSizeMacroName}]
          |  // is always considered dirty. So do not advance queue->NumSent
          |  // till AFTER data is copied.
          |
          |  size_t index = queue->numSent % ${queueSizeMacroName};
          |
          |  queue->elt[index] = *data; // Copy data into queue
          |
          |  // Release memory fence - ensure that data write above completes BEFORE we advance queue->numSent
          |  __atomic_thread_fence(__ATOMIC_RELEASE);
          |
          |  ++(queue->numSent);
          |}
          |
          |//------------------------------------------------------------------------------
          |// Receiver API
          |//
          |// See ${queueHeaderFilename} for API documentation. Only implementation details are documented here.
          |
          |void ${recvInitMethodName}(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${queueTypeName} *queue) {
          |
          |  recvQueue->numRecv = 0;
          |  recvQueue->queue = queue;
          |}
          |
          |bool ${dequeueMethodName}(
          |  ${recvQueueTypeName} *recvQueue,
          |  ${TypeUtil.eventCounterTypename} *numDropped,
          |  ${queueElementTypeName} *data) {
          |
          |  ${TypeUtil.eventCounterTypename} *numRecv = &recvQueue->numRecv;
          |  ${queueTypeName} *queue = recvQueue->queue;
          |
          |  // Get a copy of numSent so we can see if it changes during read
          |  ${TypeUtil.eventCounterTypename} numSent = queue->numSent;
          |
          |  // Acquire memory fence - ensure read of queue->numSent BEFORE reading data
          |  __atomic_thread_fence(__ATOMIC_ACQUIRE);
          |
          |  // How many new elements have been sent? Since we are using unsigned
          |  // integers, this correctly computes the value as counters wrap.
          |  ${TypeUtil.eventCounterTypename} numNew = numSent - *numRecv;
          |  if (0 == numNew) {
          |    // Queue is empty
          |    return false;
          |  }
          |
          |  // One element in the ring buffer is always considered dirty. Its the next
          |  // element we will write.  It's not safe to read it until numSent has been
          |  // incremented. Thus there are really only (${queueSizeMacroName} - 1)
          |  // elements in the queue.
          |  *numDropped = (numNew <= ${queueSizeMacroName} - 1) ? 0 : numNew - ${queueSizeMacroName} + 1;
          |
          |  // Increment numRecv by *numDropped plus one for the element we are about to read.
          |  *numRecv += *numDropped + 1;
          |
          |  // UNUSED - number of elements left to be consumed
          |  //${TypeUtil.eventCounterTypename} numRemaining = numSent - *numRecv;
          |
          |  size_t index = (*numRecv - 1) % ${queueSizeMacroName};
          |  *data = queue->elt[index]; // Copy data
          |
          |  // Acquire memory fence - ensure read of data BEFORE reading queue->numSent again
          |  __atomic_thread_fence(__ATOMIC_ACQUIRE);
          |
          |  if (queue->numSent - *numRecv + 1 < ${queueSizeMacroName}) {
          |    // Sender did not write element we were reading. Copied data is coherent.
          |    return true;
          |  } else {
          |    // Sender may have written element we were reading. Copied data may be incoherent.
          |    // We dropped the element we were trying to read, so increment *numDropped.
          |    ++(*numDropped);
          |    return false;
          |  }
          |}
          |
          |bool ${isEmptyMethodName}(${recvQueueTypeName} *recvQueue) {
          |  return (recvQueue->queue->numSent == recvQueue->numRecv);
          |}"""
    return r
  }
}