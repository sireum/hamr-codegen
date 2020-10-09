#include <camkes.h>
#include <stdio.h>
#include <assert.h>
#include <sb_Comp_A_Impl.h>

int32_t t = 0;

void Comp_A_time_triggered(const int64_t *arg){
  printf("Comp_A_time_triggered invoked.  Sending %i to Comp_B\n", t);
  sb_output_enqueue(&t);
  t++;  
}