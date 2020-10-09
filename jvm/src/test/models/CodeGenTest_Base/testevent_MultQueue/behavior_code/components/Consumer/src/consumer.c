/* apps/testevent/components/Consumer/src/main.c */

/* Similar to the "helloevent" example in CAmkES doc,
 * tidied up a bit.
 */

#include <camkes.h>
#include <stdio.h>
#include <sb_consumer_impl.h>

void testevent_consumer_component_init(const int64_t *in_arg) {
  printf("testevent_consumer_component_init called\n");
}

void testevent_consumer_s_event_handler() {
  int32_t receivedEvents = 1; // 1 for the event that caused handler to be invoked
  while(sb_s_dequeue()) {
    receivedEvents++;
  }
  
  printf("[Consumer] received %i events\n\n", receivedEvents);
}
