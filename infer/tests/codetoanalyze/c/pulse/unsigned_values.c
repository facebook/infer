/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

// integers

unsigned int returnUnsigned();

void FP_nonnegative_int_ok() {
  unsigned int x = returnUnsigned();
  if (x < 0) {
    // unreachable
    int* p = NULL;
    *p = 42;
  }
}

int returnSigned();

void signed_int_bad() {
  int x = returnSigned();
  if (x < 0) {
    // reachable
    int* p = NULL;
    *p = 42;
  }
}

// pointers to integers

unsigned int* returnUnsignedPointer();

void FP_nonnegative_int_ptr_ok() {
  unsigned int* x = returnUnsignedPointer();
  if (*x < 0) {
    // unreachable
    int* p = NULL;
    *p = 42;
  }
}

int* returnSignedPointer();

void signed_int_ptr_bad() {
  int* x = returnSigned();
  if (*x < 0) {
    // reachable
    int* p = NULL;
    *p = 42;
  }
}

// struct with integer fields

struct foo {
  unsigned int unsigned_int;
  int signed_int;
};

struct foo* returnFoo();

void FP_nonnegative_field_ok() {
  struct foo* x = returnFoo();
  if (x->unsigned_int < 0) {
    // unreachable
    int* p = NULL;
    *p = 42;
  }
}

void signed_field_bad() {
  struct foo* x = returnFoo();
  if (x->signed_int < 0) {
    // reachable
    int* p = NULL;
    *p = 42;
  }
}

// array of integers

unsigned int* returnUnsignedArray();

int FP_nonnegative_array_ok() {
  unsigned int* a = returnUnsignedArray();
  if (a[0] < 0) {
    // unreachable
    int* p = NULL;
    *p = 42;
  }
}

int* returnSignedArray();

int signed_array_bad() {
  int* a = returnSignedArray();
  if (a[0] < 0) {
    // reachable
    int* p = NULL;
    *p = 42;
  }
}
