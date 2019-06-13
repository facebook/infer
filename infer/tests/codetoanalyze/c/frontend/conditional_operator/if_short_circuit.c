/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
