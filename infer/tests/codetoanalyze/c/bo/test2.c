#include<stdlib.h>

void foo(int *arr, char *p, int i) {
  int x = arr[0];
  arr[x] = 1; /* BUG */

  *(p + i) = 0; /* BUG */
}

int bar(int argc, char**argv){
  int arr[10];
  arr[0] = 100;
  char* p = malloc(10);

  foo(arr, p, 20);

  return 0;
}
