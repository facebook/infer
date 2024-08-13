/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <stdlib.h>

void exit_example_bad() {
  int* p = NULL;
  if (p) {
    exit(1);
  }
  *p = 42;
}

void direct_exit_example_ok() {
  int* p = NULL;
  exit(1);
  *p = 42;
}

void exit_wrapper() { exit(1); }

void indirect_exit_example_ok() {
  int* p = NULL;
  exit_wrapper();
  *p = 42;
}

void direct_abort_example_ok() {
  int* p = NULL;
  abort();
  *p = 42;
}

void abort_wrapper() { abort(); }

void indirect_abort_example_ok() {
  int* p = NULL;
  abort_wrapper();
  *p = 42;
}
