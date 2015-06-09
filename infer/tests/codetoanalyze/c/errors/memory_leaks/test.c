/*
 * Copyright (c) 2013 - Facebook.
 * All rights reserved.
 */

#include <stdlib.h>

void simple_leak() {
  int *p;
  p = (int*) malloc(sizeof(int));
}

void common_realloc_leak() {
  int *p, *q;
  p = (int*) malloc(sizeof(int));
  q = (int*) realloc(p, sizeof(int) * 42);
  // if realloc fails, then p becomes unreachable
  if (q != NULL) free(q);
}

int* allocate() {
  int *p = NULL;
  do {
    p = (int*) malloc(sizeof(int));
  } while (p == NULL);
  return p;
}

void uses_allocator() {
  int *p;
  p = allocate();
  *p = 42;
}
