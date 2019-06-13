/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Basic modelling of some glib functions

#include "infer_builtins.h"

#include <stdlib.h>

// similar to malloc, but never fails, and returns NULL when size==0
void* g_malloc(size_t size) {
  if (size == 0)
    return NULL;
  void* res = malloc(size);
  INFER_EXCLUDE_CONDITION(!res);
  return res;
}

// modelled as free
void g_free(void* ptr) { free(ptr); }

void* g_realloc(void* ptr, size_t size) {
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

// simply return object, and assume it is not NULL
void* gtk_type_check_object_cast(void* object, void* cast_type) {
  if (!object)
    exit(0);
  return object;
}
