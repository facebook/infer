/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include "my_lib.h"
#include <stdlib.h>

int main() { return 0; }
int* test() { return NULL; }

int test1(int n1, int n2) { return n1 + n2; }

int test2() {
  int n1;
  int n2;
  return n1 + n2;
}

int test3(int* ptr) { return *ptr; }

int foo();

int test4() { return foo(); }

int test5() { return bar(); }
