#include <all.h>
#include <errno.h>
#include <sys/msg.h>
#include <time.h>

// This file is auto-generated.  Do not edit

// sleeps ns nanoseconds, resuming after signals (the C scheduler installs handlers)
static void sleep_ns(int64_t ns) {
  struct timespec req = { .tv_sec = (time_t) (ns / 1000000000), .tv_nsec = (long) (ns % 1000000000) };
  struct timespec rem;
  while (nanosleep(&req, &rem) == -1 && errno == EINTR) req = rem;
}

struct Message {
  long mtype;
  union art_DataContent data;
};

static int msqid = 0;

Z PACKAGE_NAME_MessageQueue_create(STACK_FRAME Z msgid) {
  unsigned int permission = 0666;
  unsigned int mask = IPC_CREAT;
  msqid = msgget((key_t) msgid, (int) (permission | mask));
  return (Z) msqid;
}

Unit PACKAGE_NAME_MessageQueue_remove(STACK_FRAME Z msgid) {
  msgctl((int) msgid, IPC_RMID, NULL);
}

void PACKAGE_NAME_MessageQueue_receive(STACK_FRAME Tuple2_D0E3BB result) {
  struct Message r;
  msgrcv(msqid, &r, sizeof(union art_DataContent), 0, 0);
  result->type = TTuple2_D0E3BB;
  result->_1 = (Z) r.mtype;
  Type_assign(&result->_2, &r.data, sizeOf((Type) &r.data));
}

Unit PACKAGE_NAME_MessageQueue_send(STACK_FRAME Z msgid, Z port, art_DataContent d) {
  struct Message m = { .mtype = port, .data = *d };
  msgsnd(msgget((key_t) msgid, 0644), &m, sizeof(union art_DataContent), 0);
}

// n is in nanoseconds (Art.Time)
Unit PACKAGE_NAME_Process_sleep(STACK_FRAME S64 n) {
  sleep_ns(n);
}
