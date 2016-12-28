#include<stdlib.h>

void arr_access(int *arr, char *p, int i) {
  int x = arr[0];
  arr[x] = 1;			/* BUG */
  *(p + i) = 'a';		/* BUG */
}

void function_call(){
  int arr[10];
  arr[0] = 100;
  char* p = malloc(10);
  arr_access(arr, p, 20);
}
