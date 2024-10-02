#include <all.h>

#include <sys/time.h>
#include <time.h>

/** Returns current system time in milliseconds
  * NOTE: this requires returning 64bit ints
  */
Z art_Process_time(STACK_FRAME_ONLY) {
  struct timeval tv; //Get a time structure
  gettimeofday(&tv, NULL); //Get the current time
  int64_t t = tv.tv_sec;
  t *= 1000;
  t += tv.tv_usec/1000;
  return  t;
}