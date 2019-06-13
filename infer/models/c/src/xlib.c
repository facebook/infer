/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Basic modelling of some xlib functions

#include "infer_builtins.h"

#include <stdlib.h>

// modelled using malloc
char* XGetAtomName(void* display, void* atom) {
  int size;
  INFER_EXCLUDE_CONDITION(size <= 0);
  return (char*)malloc(size);
}

// modelled as free, requires NONNULL pointer
void XFree(void* ptr) {
  INFER_EXCLUDE_CONDITION(!ptr);
  free(ptr);
}
