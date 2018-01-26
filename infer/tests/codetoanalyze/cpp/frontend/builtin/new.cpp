/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void test() {
  int x = 2;
  int* i = new int;
  new int; // this will be leak - test that it gets to cfg
  delete i;

  // int* i_a = new int[10];
  // delete[] i_a;
}

struct A {};

void* operator new(unsigned long size, void* ptr, void* ptr2) noexcept {
  return ptr2;
};

void test_placement(void* ptr, int* ptr2) { auto* p = new (ptr, ++ptr2) A(); }
