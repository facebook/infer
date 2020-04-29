/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

void call_my_realloc_array_ok(int* p) {
  p = my_realloc(p, 10 * sizeof(int));
  p[0] = 42;
  free(p);
}

int call_call_my_realloc_array_ok() {
  int* p;
  p = xmalloc(sizeof(int));
  call_my_realloc_array_ok(p);
}
