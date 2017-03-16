/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include "lib1.h"
#include "lib2.h"

void test0() {
  int* s = nullptr; // requires -std=c++11 flag
  *s = 42;
}

int test1() {
  int* s = nullptr;
  return deref1(s); // should be nullderef
}

int test2() {
  int* s = nullptr;
  return deref2(s); // should be nullderef, but will be skipped
  //  because of --changed-files-index option
}
