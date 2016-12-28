#include <stdio.h>
#include <stdlib.h>

char* safealloc(n) {
  char* x;
  if(n > 0)
    x = malloc(n);
  else
    x = malloc(10);

  if(!x) 
    return x; 
  else  
    exit(1);
 }

void do_while(char* a, int len) {
  int i = 0;
  do {
    a[i] = i;
    i++;
  } while(i < len);
}


void arr_access(int *arr, char *p, int i) {
  int x = arr[0];
  arr[x] = 1;			/* BUG */
  *(p + i) = 'a';		/* BUG */
}

int main() {
  int i = 0;
  int j = 0;

  /* trivial */
  int arr[10];
  arr[10] = 0;			/* BUG */

  /* function call with array pointer */
  arr[0] = 100;
  char* p = malloc(10);
  arr_access(arr, p, 20);

  /* for loop */
  char *y = safealloc(10);
  for(i = 0; i < 10; i++) {
    y[i] = 'a';			/* SAFE */
  }
  y = safealloc(5);
  for(i = 0; i < 10; i++) {
    y[i] = 'a';			/* BUG */
  }

  /* while */
  i = 0;
  y = safealloc(10);
  while(*(y + i) && i < 10)	/* BUG */
    y[i++] = 1;			/* SAFE */

  /* do-while */
  char* a = malloc(10);
  do_while(a, 10);		/* SAFE */
  do_while(a, 11);		/* BUG */

  /* nested loop */
  for(i = 0; i < 10; i++){
    a[i] = 'a';			/* SAFE */
    for(j = 0; j <= 10; j++){
      a[j] = 'a';		/* BUG */
    }
  }

  /* break, continue, return */
  i = 0;
  while(1){
    i++;
    if(i >= 10) break;
    if(i < 2) continue;
    a[i] = 'a';			/* SAFE */
  }
  i = 0;
  while(1){
    if(i > 10) return 0;
    a[i] = 'a';			/* BUG */
    i++;
  }

  /* nested loop with label */
  j = 0;
  for(i = 0; i < 10; i++){
  outer_loop:
    a[j] = 'a';			/* BUG */
    for(j = 0; j <= 10; j++){
      if(j >= 10) goto outer_loop;
    }
  }
  
  /* loop using goto */
  i = 0;
 loop_start:
  if(i >= 10) goto loop_end;
  a[i] = 'a';			/* SAFE */
  i++;
  goto loop_start;
 loop_end:
  a[i] = 'a';			/* BUG */

  /* infinite loop */
  i = 0;
  while(1){
    if(i >= 10) i = 0;
    a[i] = 'a';			/* SAFE */
    i++;
  }
}
