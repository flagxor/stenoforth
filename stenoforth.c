#include "stenoforth.h"
#include <stdlib.h>
#include <string.h>

#if DEBUG_MODE
const char *opname[] = {
#define X(sname, name, code) sname,
PRIMITIVE_LIST
#if DEBUG_WORDS
PRIMITIVE_LIST_DEBUG
#endif
#undef X
};
#endif

enum {
#define X(sname, name, code) OP_ ## name,
PRIMITIVE_LIST
#if DEBUG_WORDS
PRIMITIVE_LIST_DEBUG
#endif
#undef X
};

#if DEBUG_MODE
char *findname(int32_t *x) {
  static char buffer[100];
  int count = x[-1] & 0xff;
  int padding = (count + 3) & ~3;
  char *end = (char *) &x[-3];
  strcpy(buffer, "");
  strncpy(buffer, end - padding, count);
  buffer[count] = 0;
  return buffer;
}
#endif

#if DEBUG_WORDS
static void print_hexadecimal(cell_t value) {
  char tmp[40];
  sprintf(tmp, " %"PRIxPTR, value);
  const char *pos = tmp;
  while (*pos) {
    emit(*pos++);
  }
}

static void print_decimal(cell_t value) {
  char tmp[40];
  sprintf(tmp, " %"PRIdPTR, value);
  const char *pos = tmp;
  while (*pos) {
    emit(*pos++);
  }
}
#endif

cell_t *vm(cell_t *initrp) {
  cell_t *rp = initrp;
  int32_t *ip = (int32_t *) *rp--;
  cell_t *sp = (cell_t *) *rp--;
  cell_t tos = *sp--;
  cell_t w = 0;
  cell_t t = 0;
  cell_t ir;
  //dcell_t d, n, m;
  for (;;) {
#if DEBUG_MODE
    printf("sp: %p, rp: %p\n", sp, rp);
    printf("stack: %d %d %d %d\n", (int) sp[-2], (int) sp[-1], (int) sp[0], (int) tos);
    printf("rstack: %d\n", (int) rp[0]);
#endif
    w = *ip;
#if DEBUG_MODE
    printf("w: %p\n", (cell_t*)w);
#endif
    for (;;) {
      ir = ip[w];
      ++ip;
#if DEBUG_MODE
      printf("ir: %p -- %s\n", (cell_t*)ir, findname(&ip[w - 1]));
#endif
      switch (ir & 0xff) {
#define X(sname, name, code) case OP_ ## name: code; break;
        PRIMITIVE_LIST
#if DEBUG_WORDS
        PRIMITIVE_LIST_DEBUG
#endif
#undef X
        default:
          break;
      }
      break;
    }
  }
}

