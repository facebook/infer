/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

namespace dead_stores {

void easy_bad() { int x = 5; }

void reassign_param_bad(int x) { x = 5; }

int dead_then_live_bad() {
  int x = 5;
  x = 3;
  return x;
}

int use_then_dead_bad() {
  int x = 5;
  int y = x;
  x = 7;
  return y;
}

void dead_pointer_bad() {
  int num = 2;
  int* x = &num;
}

void plus_plus1_bad() {
  int i = 0;
  ++i;
}

void plus_plus2_bad() {
  int i = 0;
  i++;
}

int plus_plus3_bad() {
  int i = 0;
  return i++;
}

void FN_capture_no_read_bad() {
  int x = 0;
  [x]() { return; }();
}

void FN_init_capture_no_read_bad() {
  [i = 0]() { return; };
}

int return_ok() {
  int x = 5;
  return x;
}

int branch_ok(bool b) {
  int x = 5;
  int y = 3;
  if (b) {
    y = x;
  }
  return y;
}

int loop_ok(bool b) {
  int x = 5;
  int y = 3;
  while (b) {
    y = x;
    b = false;
  }
  return y;
}

int loop_break_ok(bool b) {
  int x = 5;
  while (b) {
    x = 3;
    break;
  }
  return x;
}

int loop_continue_ok(bool b) {
  int x = 5;
  int y = 2;
  while (b) {
    y = x;
    x = 3;
    continue;
  }
  return y;
}

void assign_pointer1_ok(int* ptr) { *ptr = 7; }

int* assign_pointer2_ok() {
  int num = 2;
  int* ptr = &num;
  return ptr;
}

void by_ref1_ok(int& ref) { ref = 7; }

void by_ref2_ok(int& ref) { ref++; }

int plus_plus_ok() {
  int x = 0;
  return ++x;
}

int plus_plus_loop_ok(int n) {
  int i;
  for (i = 0; i < n; i++) {
    i++;
  }
  return i;
}

void capture1_ok() {
  int x = 1;
  [x]() { return x; }();
}

void capture2_ok(int x) {
  [x]() { return x; }();
}

int FP_capture_by_ref_ok() {
  int x = 0;
  [&x]() { x++; }();
  return x;
}

void init_capture_ok() {
  [i = 0]() { return i; };
}

char* global;

void FP_assign_array_tricky_ok() {
  char arr[1];
  global = arr;
  *(int*)arr = 123; // think this is a bug in the frontend... this instruction
  // looks like &arr:int = 123
}
}
