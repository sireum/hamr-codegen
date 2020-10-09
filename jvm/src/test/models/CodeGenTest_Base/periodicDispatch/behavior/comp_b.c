#include <camkes.h>
#include <stdio.h>
#include <assert.h>

void Comp_B_input(const int32_t * in_arg){
  printf("Comp_B_input received event %i\n", *in_arg);
}