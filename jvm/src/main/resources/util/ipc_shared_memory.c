#include <all.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/shm.h>
#include <sys/sem.h>
#include <unistd.h>

// This file is auto-generated.  Do not edit

// System V IPC keys are this system's base key plus the port id, so that different systems, and
// stale objects left by other systems, do not share keys.  The base is derived from the system's
// package name and is never 0 (IPC_PRIVATE).  Set HAMR_IPC_KEY_BASE to override it, e.g. to run
// two copies of the same system at once
#define IPC_KEY_BASE_DEFAULT IPC_KEY_BASE_VALUE

static key_t ipc_key(Z port) {
    static long base = -1;
    if (base < 0) {
        const char *env = getenv("HAMR_IPC_KEY_BASE");
        base = (env != NULL) ? strtol(env, NULL, 0) : IPC_KEY_BASE_DEFAULT;
    }
    return (key_t) (base + port);
}

// Aborts with the failed call, the port, its key and the reason
static void ipc_fail(STACK_FRAME const char *call, Z port) {
    DeclNewStackFrame(caller, "ipc.c", "SharedMemory", call, 0);
    char msg[256];
    snprintf(msg, sizeof(msg), "%s failed for port %lld (System V IPC key 0x%lx): %s",
             call, (long long) port, (unsigned long) ipc_key(port), strerror(errno));
    sfAbort(msg);
}

static void sem_op(STACK_FRAME int sid, short val, Z port) {
    struct sembuf sem_op;
    sem_op.sem_num = 0;
    sem_op.sem_op = val;
    sem_op.sem_flg = 0;
    if (semop(sid, &sem_op, 1) == -1) ipc_fail(CALLER "semop", port);
}

static void lock(STACK_FRAME int sid, Z port) {
    sem_op(CALLER sid, -1, port);
}

static void unlock(STACK_FRAME int sid, Z port) {
    sem_op(CALLER sid, 1, port);
}

static int get_sem(STACK_FRAME Z port) {
    int sid = semget(ipc_key(port), 1, 0666);
    if (sid == -1) ipc_fail(CALLER "semget", port);
    return sid;
}

static Option_8E9F45 attach(STACK_FRAME Z port) {
    int shmid = shmget(ipc_key(port), sizeof(union Option_8E9F45), 0666);
    if (shmid == -1) ipc_fail(CALLER "shmget", port);
    void *p = shmat(shmid, (void *) 0, 0);
    if (p == (void *) -1) ipc_fail(CALLER "shmat", port);
    return (Option_8E9F45) p;
}

static void create_sem(STACK_FRAME Z port) {
    int sem_set_id = semget(ipc_key(port), 1, IPC_CREAT | 0666);
    if (sem_set_id == -1) ipc_fail(CALLER "semget", port);
    union semun {
        int val;
        struct semid_ds *buf;
        ushort *array;
    } sem_val;
    sem_val.val = 1;
    if (semctl(sem_set_id, 0, SETVAL, sem_val) == -1) ipc_fail(CALLER "semctl", port);
}

Z PACKAGE_NAME_SharedMemory_create(STACK_FRAME Z id) {
    create_sem(CALLER id);

    int shmid = shmget(ipc_key(id), sizeof(union Option_8E9F45), IPC_CREAT | 0666);
    if (shmid == -1) ipc_fail(CALLER "shmget", id);
    void *p = shmat(shmid, (void *) 0, 0);
    if (p == (void *) -1) ipc_fail(CALLER "shmat", id);
    memset(p, 0, sizeof(union Option_8E9F45));
    shmdt(p);

    return (Z) shmid;
}

// MBox2_43CC67=MBox2[art.Art.PortId, art.DataContent]
Unit PACKAGE_NAME_SharedMemory_receive(STACK_FRAME Z port, MBox2_43CC67 out) {
    int sid = get_sem(CALLER port);

    lock(CALLER sid, port);

    Option_8E9F45 p = attach(CALLER port);

    while (p->type != TSome_D29615) { // wait until there is data
        unlock(CALLER sid, port);
        usleep((useconds_t) 10 * 1000);
        lock(CALLER sid, port);
    }

    art_DataContent d = &p->Some_D29615.value;
    Type_assign(&(out->value2), d, sizeOf((Type) d));
    memset(p, 0, sizeof(union Option_8E9F45));
    shmdt(p);

    unlock(CALLER sid, port);
}

// MBox2_37E193=MBox2[art.Art.PortId, Option[art.DataContent]]
Unit PACKAGE_NAME_SharedMemory_receiveAsync(STACK_FRAME Z port, MBox2_37E193 out) {
    int sid = get_sem(CALLER port);

    lock(CALLER sid, port);

    Option_8E9F45 p = attach(CALLER port);

    if (p->type == TSome_D29615) {
        Type_assign(&(out->value2), p, sizeOf((Type) p));
        memset(p, 0, sizeof(union Option_8E9F45));
    } else {
        out->value2.type = TNone_964667;
    }

    shmdt(p);

    unlock(CALLER sid, port);
}

Unit PACKAGE_NAME_SharedMemory_send(STACK_FRAME Z appPortId, Z componentPortId, art_DataContent d) {
    int sid = get_sem(CALLER componentPortId);

    lock(CALLER sid, componentPortId);

    Option_8E9F45 p = attach(CALLER componentPortId);

    while (p->type == TSome_D29615) {
        unlock(CALLER sid, componentPortId);
        usleep((useconds_t) 10 * 1000);
        lock(CALLER sid, componentPortId);
    }

    p->type = TSome_D29615;
    Type_assign(&(p->Some_D29615.value), d, sizeOf((Type) d));

    shmdt(p);

    unlock(CALLER sid, componentPortId);
}

B PACKAGE_NAME_SharedMemory_sendAsync(STACK_FRAME Z appPortId, Z componentPortId, art_DataContent d) {
    int sid = get_sem(CALLER componentPortId);

    lock(CALLER sid, componentPortId);

    Option_8E9F45 p = attach(CALLER componentPortId);
    p->type = TSome_D29615;
    Type_assign(&(p->Some_D29615.value), d, sizeOf((Type) d));

    shmdt(p);

    unlock(CALLER sid, componentPortId);
    return T;
}

Unit PACKAGE_NAME_SharedMemory_remove(STACK_FRAME Z id) {
    semctl(semget(ipc_key(id), 1, 0666), 0, IPC_RMID);
    shmctl(shmget(ipc_key(id), sizeof(union Option_8E9F45), 0666), IPC_RMID, NULL);
}

Unit PACKAGE_NAME_Process_sleep(STACK_FRAME Z n) {
    usleep((useconds_t) n * 1000);
}
