#include "stenoforth.h"
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
  if (argc == 2 && strcmp(argv[1], "js") == 0) {
    int i = 0;
#define X(sname, name, code) printf("case %d: %s; break;\n", i++, #code);
    PRIMITIVE_LIST
#if DEBUG_WORDS
    PRIMITIVE_LIST_DEBUG
#endif
#undef X
  } else if (argc == 2 && strcmp(argv[1], "ops") == 0) {
    int i = 0;
#define X(sname, name, code) printf("%d %s\n", i++, sname);
    PRIMITIVE_LIST
#if DEBUG_WORDS
    PRIMITIVE_LIST_DEBUG
#endif
#undef X
  } else {
    fprintf(stderr, "Usage: %s js/ops\n", argv[1]);
    return 1;
  }
}
