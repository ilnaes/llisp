#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>

#define TRUE 0x0000000000000002L
#define FALSE 0x0000000000000000L

#define NUM_MIN (- (1L << 62))
#define NUM_MAX ((1L << 62) - 1)

extern int64_t our_main() asm("_our_main");
extern int64_t print(int64_t) asm("_print");
extern void error(int64_t) asm("_error");

void error(int64_t val) {
    switch(val) {
        case 0: fprintf(stderr, "overflow"); break;
    }
    exit(1);
}

int64_t print(int64_t val) {
  if ((val & 0x1L) == 0) {
    if (val == TRUE)
      fprintf(stdout, "%s\n", "true");
    else if(val == FALSE)
      fprintf(stdout, "%s\n", "false");
    else {
      fprintf(stderr, "FALSE: %" PRId64 "\n", val);
      exit(1);
    }
  } else {
    fprintf(stdout, "%" PRId64 "\n", val >> 1);
  }
  return val;
}


int main() {
  int64_t result = our_main();
  print(result);
  return 0;
}
