/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void init(int* i) { *i = 10; }

void init_bool(bool* i) { *i = false; }

void no_init(int* i) { i = 0; }

void no_init_bool(bool* i) { i = 0; }

int inc(int x) { return x++; }
// error is detected before call as we copy x
// so no need to put it in the summary

int no_init_return_bad() {
  int x;
  return x; // error
}

void bad1() {
  int a;
  int b = a; // Error
  int c = b; // Error but we do not report as it depends from line 20
}

int bad2() {
  int a;
  int b = 0;
  int c = 0;

  no_init(&a);

  b = a; // error
}

int bad3() {
  int a;
  int b = 0;
  int c = 0;

  no_init(&a);

  c = inc(a); // error
}

int ok1() {
  int a;
  int b = 0;
  int c = 0;

  init(&a);
  c = a; // OK
}

int ok2() {
  int a;
  int b = 0;
  int c = 0;

  init(&a);

  c = inc(a); // ok
}

int bad4() {
  int a;
  int b = 0;
  int c = 0;

  no_init(&a);

  b = a; // report here error

  c = inc(b); // do not report as it depends from line 31

  return 0;
}

// this function shows that we correctly reportat
// line 88 but not report the error at line 90
int bad5() {
  int a;
  int b = 0;
  int c = 0;

  no_init(&a);

  b = a; // error

  return b; // should not report as it depends from line 31
}

// this shows that in case a function return an uninit value, it gets the blame
// rather than the caller.
int blame_on_callee() {
  int a;
  int c = no_init_return_bad();

  a = c; // we don't flag the error here as it is flagged in no_init_return
         // definition

  return 0;
}

void maybe_init(int y, int* formal) {

  if (y == 0) {
    *formal = 5;
  };
}

void must_init(int y, int* formal) {

  if (y == 0) {
    *formal = 5;
  } else {
    *formal = 17;
  };
}

int call_maybe_init_bad(int y) {
  int x;
  maybe_init(y, &x);
  return x;
}

int call_must_init_ok(int y) {
  int x;
  must_init(y, &x);
  return x;
}

void square_init(int x, int& res) { res = x * x; }

int square_no_init(int x, int& res) { return res * res; }

void use_square_OK() {

  int i;
  square_init(2, i);
}

void use_square_bad() {

  int i;
  i = square_no_init(2, i); // Error
}

int no_deref(int* x) {
  int* y = 0;
  x = y;
  return *x; // this is not actually a deref of a formal
}

void init_x(int* x) {
  int* y;
  y = x;
  *y = 25; // this is writing x
}

int use_init_x_OK() {

  int a;
  init_x(&a);

  return a;
}

void bool1_bad() {
  bool a;
  bool b = a;
  bool c = b;
}

int bool2_bad() {
  bool a;
  bool b = 0;
  bool c = 0;

  no_init_bool(&a);

  b = a; // error
}

int bool1_ok() {
  bool a;
  bool b = 0;
  bool c = 0;

  init_bool(&a);
  c = a; // OK
}
