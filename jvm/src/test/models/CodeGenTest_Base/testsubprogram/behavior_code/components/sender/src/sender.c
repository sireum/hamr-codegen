/* testsubprogram/components/sender/src/sender */

#include <camkes.h>
#include <sb_types.h>
#include "../includes/sb_sender_impl.h"

void sender_init(const int64_t *arg){
   printf("Initializer method for sender invoked\n");
}

void run_sender(int64_t * arg) {
   uint32_t result;

   operations_add(10, 5, &result);
   printf("Result of 'add' call to receiver with arguments 10, 5 : (%d) \n", result);
   
   operations_subtract(10, 5, &result);  
   printf("Result of 'subtract' call to receiver with arguments 10, 5 : (%d) \n", result);
}
