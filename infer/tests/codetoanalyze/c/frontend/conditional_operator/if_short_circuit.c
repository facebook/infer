/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <stdlib.h>

void shortcircuit_or(int* x) {
  // x = 0;
  if (x == 0 || *x == 2) {
    x = 17;
  } else {
    x = 32;
  };
}

void shortcircuit_and(int* x) {
  if (!x && !(x = getenv("BLOCK"))) {
    x = 17;
  } else {
    *x = 32;
  };
}

void test_loop() {

  char* spec;
  char* block_size;

  spec = getenv("BLOCK");

  while ((!spec && !(spec = getenv("BLOCK_SIZE")) &&
          !(spec = getenv("BLOCKSIZE")))) {
    block_size = 0;
  }
}

int main() {

  char* spec;
  char* block_size;

  spec = getenv("BLOCK");

  if (!spec && !(spec = getenv("BLOCK_SIZE")) && !(spec = getenv("BLOCKSIZE")))
    block_size = 0;
  else {
    if (*spec == '\'')
      block_size = 0;
  }

  return 0;
}
