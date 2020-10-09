/* testsubprogram/components/receiver/src/receiver */

#include <camkes.h>
#include <sb_types.h>
#include "../includes/sb_receiver_impl.h"

void operations_add(uint32_t A, uint32_t B, uint32_t *result) {
	*result = A + B;
}

void operations_subtract(uint32_t A, uint32_t B, uint32_t *result) {
	*result = A - B;
}
