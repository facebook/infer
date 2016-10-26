/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>
#include <string.h>

void simple_leak() {
  int* p;
  p = (int*)malloc(sizeof(int));
}

void common_realloc_leak() {
  int *p, *q;
  p = (int*)malloc(sizeof(int));
  q = (int*)realloc(p, sizeof(int) * 42);
  // if realloc fails, then p becomes unreachable
  if (q != NULL)
    free(q);
}

int* allocate() {
  int* p = NULL;
  do {
    p = (int*)malloc(sizeof(int));
  } while (p == NULL);
  return p;
}

void uses_allocator() {
  int* p;
  p = allocate();
  *p = 42;
}

void* builtin_no_leak(size_t s) {

  char* str = malloc(sizeof(s));
    if (str) {
    return memset(str, 0, s);
    }
}

void conditional_last_instruction() {
  int* p = malloc(sizeof(int));
  if (0) {
    free(p);
  }
}

int* compound_return_no_leak() {
  return ({
    int* p = malloc(sizeof(int));
    p;
  });
}
