#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TRUE 0x000000000000000AL
#define FALSE 0x0000000000000002L
#define MASK 0xFFFFFFFFFFFFFFF8L
#define HEAP_SIZE 100000

#define NUM_MIN (-(1L << 62))
#define NUM_MAX ((1L << 62) - 1)

extern int64_t our_main();

extern int64_t *new (int64_t);
extern void print(int64_t);
extern void error(int64_t);

int64_t *mem;
int64_t alloced;

void error(int64_t val) {
  switch (val) {
  case 0:
    fprintf(stderr, "overflow");
    break;
  }
  exit(1);
}

void pr(int64_t val) {
  if ((val & 0x1L) == 1) {
    fprintf(stdout, "%" PRId64, val >> 1);
  } else {
    if (val == TRUE)
      fprintf(stdout, "%s", "true");
    else if (val == FALSE)
      fprintf(stdout, "%s", "false");
    else if ((val & 0x7) == 0) {
      fprintf(stdout, "( ");
      int64_t *ptr = (int64_t *)(val & MASK);
      int64_t n = *ptr;

      for (int i = 0; i < n; i++) {
        pr(ptr[i + 1]);
        if (i < n - 1) {
          fprintf(stdout, ", ");
        } else {
          fprintf(stdout, " ");
        }
      }
      fprintf(stdout, ")");
    } else {
      fprintf(stderr, "UNKNOWN: %" PRId64 "\n", val);
      exit(1);
    }
  }
}

void print(int64_t val) {
  pr(val);
  fprintf(stdout, "\n");
}

int64_t *new (int64_t size) {
  int64_t *res = mem;
  alloced += size;

  if (alloced > HEAP_SIZE) {
    fprintf(stderr, "Heap exceeded\n");
    exit(1);
  }

  mem += size;
  return res;
}

int main() {
  mem = calloc(HEAP_SIZE, sizeof(int64_t));
  int64_t result = our_main();
  print(result);
  return 0;
}
