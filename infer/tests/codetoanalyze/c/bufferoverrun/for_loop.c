#include <stdlib.h>

char* safealloc(int n) {
  char* x;
  if (n > 0)
    x = malloc(n);
  else
    x = malloc(10);

  if (!x)
    return x;
  else
    exit(1);
}

void for_loop() {
  char* a;
  int i;

  a = safealloc(10);
  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* SAFE */
  }
  a = safealloc(5);
  for (i = 0; i < 10; i++) {
    a[i] = 'a'; /* BUG */
  }
}
