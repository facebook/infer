/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stddef.h>

// builtin: return the size of arr
extern size_t __get_array_length(const void* arr);

// builtin: change the size of the array to size
extern void __set_array_length(void* ptr, size_t size);

void* my_realloc(void* ptr, size_t size) {
  if (size == 0) { // return NULL and free ptr unless it is NULL
    if (ptr)
      free(ptr);
    return NULL;
  }
  int old_size;
  old_size = __get_array_length(ptr); // force ptr to be an array
  int can_enlarge; // nondeterministically choose whether the current block can
  // be enlarged
  if (can_enlarge) {
    __set_array_length(ptr, size); // enlarge the block
    return ptr;
  }
  int* newblock = (int*)malloc(size);
  if (newblock) {
    free(ptr);
    return newblock;
  } else
    exit(0); // assume that new allocation does not fail
}

void foo(int* p) {
  p = my_realloc(p, 10 * sizeof(int));
  p[0] = 42;
  free(p);
}

int main() {

  int* p;
  p = xmalloc(sizeof(int));
  foo(p);
}
