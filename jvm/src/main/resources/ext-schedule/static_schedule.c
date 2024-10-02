#include <all.h>

__attribute__((weak)) art_scheduling_static_Schedule_DScheduleSpec PACKAGE_NAME_RunStaticScheduleDemo_schedule(STACK_FRAME_ONLY);

void PACKAGE_NAME_StaticScheduleProvider_getStaticSchedule(STACK_FRAME art_scheduling_static_Schedule_DScheduleSpec result){

    if(PACKAGE_NAME_RunStaticScheduleDemo_schedule) {
        art_scheduling_static_Schedule_DScheduleSpec schedule = PACKAGE_NAME_RunStaticScheduleDemo_schedule();
        result->hyperPeriod = schedule->hyperPeriod;
        result->maxDomain = schedule->maxDomain;
        memcpy(&result->schedule, &schedule->schedule, sizeof(struct art_scheduling_static_Schedule_DSchedule));
    } else {
        printf("RunStaticScheduleDemo.schedule not found.  You'll need to supply your own order in C\n");
        exit(-1);
    }

}