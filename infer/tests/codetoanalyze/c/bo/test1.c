#include <stdio.h>
#include <stdlib.h>
#define A 10

char* safealloc(n) {
  char* x;
  if (n>0) { x = malloc(n);} else {x = malloc(10);};
  if (!x) 
    return x; 
  else  
    exit(1);
 }

char foo(char* a, int n) { 
  return 'a';
}

int main() {
  int i = 0;
  char *y = safealloc(A);
  for (i=0;i<A; i++) {
    y[i] = foo(y, i); /* OK */
  }
  i = 0;
  while (*(y+i) && i < A) /* BAD */
    y[++i] = 1;  /* BAD */
}
