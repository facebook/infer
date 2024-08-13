/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// integers

unsigned int returnUnsigned();
void nonnegative_int() {
  unsigned int x = returnUnsigned();
  if (x < 0) {
    int y = x / 0; // shouldn't report
  }
}

int returnSigned();
void signed_int() {
  int x = returnSigned();
  if (x < 0) {
    int y = 1 / 0; // should report
  }
}

// pointers to integers

unsigned int* returnUnsignedPointer();
void nonnegative_int_ptr() {
  unsigned int* x = returnUnsignedPointer();
  if (*x < 0) {
    int y = 1 / 0; // shouldn't report
  }
}

int* returnSignedPointer();
void signed_int_ptr() {
  int* x = returnSigned();
  if (*x < 0) {
    int y = 1 / 0; // should report
  }
}

// struct with integer fields

struct foo {
  unsigned int unsigned_int;
  int signed_int;
};

struct foo* returnFoo();

void nonnegative_field() {
  struct foo* x = returnFoo();
  if (x->unsigned_int < 0) {
    int y = 1 / 0; // shouldn't report
  }
}

void signed_field() {
  struct foo* x = returnFoo();
  if (x->signed_int < 0) {
    int y = 1 / 0; // should report
  }
}

// array of integers

unsigned int* returnUnsignedArray();
int nonnegative_array() {
  unsigned int* a = returnUnsignedArray();
  if (a[0] < 0) {
    int y = 1 / 0; // shouldn't report
  }
}

int* returnSignedArray();
int signed_array() {
  int* a = returnSignedArray();
  if (a[0] < 0) {
    int y = 1 / 0; // should report
  }
}
