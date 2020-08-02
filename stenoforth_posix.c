#include "stenoforth.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

struct termios old = {0};

static void cleanup(void) {
  if (tcsetattr(0, TCSADRAIN, &old) < 0)
    perror("tcsetattr ~ICANON");
}

void emit(cell_t ch) {
  if (ch == 12) {
    printf("\x1b[2J\x1b[H");
    fflush(stdout);
    return;
  }
  fputc(ch, stdout);
  fflush(stdout);
}

cell_t qkey(void) {
  return fgetc(stdin);
}

void color(cell_t c) {
  if (c < 0) {
    printf("\x1b[0m");
  } else {
    printf("\x1b[38;5;%dm", (int) c);
  }
  fflush(stdout);
}

int main(int argc, char *argv[]) {
  cell_t *rp = vm_load(argv[0]);

  // Setup Terminal
  if (tcgetattr(0, &old) < 0) {
    perror("tcsetattr()");
  }
  if (tcsetattr(0, TCSANOW, &old) < 0) {
    perror("tcsetattr ICANON");
  }
  struct termios new = old;
  new.c_lflag &= ~ICANON;
  new.c_lflag &= ~ECHO;
  new.c_cc[VMIN] = 1;
  new.c_cc[VTIME] = 0;
  if (tcsetattr(0, TCSADRAIN, &new) < 0)
    perror ("tcsetattr ~ICANON");
  atexit(cleanup);

  for (;;) {
    rp = vm(rp);
  }
  return 0;
}

