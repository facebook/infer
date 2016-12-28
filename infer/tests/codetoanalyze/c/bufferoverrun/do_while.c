#include <stdlib.h>

void do_while_sub(char* a, int len) {
  int i = 0;
  do {
    a[i] = i;
    i++;
  } while (i < len);
}

void do_while() {
  char* a = malloc(10);
  do_while_sub(a, 10); /* SAFE */
  do_while_sub(a, 11); /* BUG */
}
