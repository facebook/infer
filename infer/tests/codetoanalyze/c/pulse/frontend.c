/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

void assign_implicit_cast_ok() {
  bool* b = (bool*)malloc(sizeof(bool));
  uint16_t i = 1;
  if (b) {
    *b = true;
    *b = !i;
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void assign_implicit_cast_bad() {
  bool* b = (bool*)malloc(sizeof(bool));
  uint16_t i = 0;
  if (b) {
    *b = false;
    *b = !i;
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void assign_paren_ok() {
  bool* b = (bool*)malloc(sizeof(bool));
  int x = 42, y = 33;
  if (b) {
    *b = true;
    *b = (x == y);
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}

void assign_paren_bad() {
  bool* b = (bool*)malloc(sizeof(bool));
  int x = 42, y = 42;
  if (b) {
    *b = false;
    *b = (x == y);
    if (*b) {
      int* p = 0;
      *p = 5;
    }
    free(b);
  }
}
