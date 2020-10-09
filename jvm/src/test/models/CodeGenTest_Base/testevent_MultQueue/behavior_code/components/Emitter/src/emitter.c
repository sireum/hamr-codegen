/* testevent/components/emitter/src/emitter.c */

#include <camkes.h>
#include <stdio.h>
#include <sb_types.h>
#include <sb_emitter_impl.h>

static int32_t counter = 0;

void testevent_emitter_component_init(const int64_t *in_arg)
{
    printf("testevent_emitter_component_init called\n");
}

/* control thread: keep calling enqueue for thing
 */
void run_emitter(const int64_t *in_arg){
  int numEvents = counter % 7; // send 0 - 6 events per dispatch, consumer's queue size is 5
  for(int32_t i = 0; i < numEvents; i++) {
    sb_e_enqueue();
  }
  printf("[Emitter] Sent %i events.\n", numEvents);
   
  counter++;
}
