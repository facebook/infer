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

void no_init(int* i) {}

void no_init_bool(bool* i) {}

int inc(int x) { return x + 1; }
// error is detected before call as we copy x
// so no need to put it in the summary

int no_init_return_bad() {
  int x;
  return x; // error
}

int bad1() {
  int a;
  int b = a; // Error
  int c = b; // Error but we do not report as it depends from line 20
  return c;
}

int ok1() {
  int a;
  int b;
  no_init(&a);

  b = a; // OK (only intraprocedural)
  return b;
}

int ok2() {
  int a;
  int c;
  no_init(&a);

  c = inc(a); // OK (only intraprocedural)
  return c;
}

int ok3() {
  int a;
  int c;

  init(&a);
  c = a; // OK

  return c;
}

int ok4() {
  int a;
  int c;

  init(&a);

  c = inc(a); // ok
  return c;
}

int ok5() {
  int a;
  int b;
  int c;

  no_init(&a);

  b = a; // ok (only intraprocedural)

  c = inc(b); // do not report as it depends from line 31

  return c;
}

void square_init(int x, int& res) { res = x * x; }

int square_no_init(int x, int& res) { return res * res; }

void use_square_ok1() {

  int i;
  square_init(2, i);
}

int use_square_ok2() {

  int i;
  i = square_no_init(2, i); // OK only intraprocedural
  return i;
}

bool getOK(void);

int branch1_FP() {

  int size;

  bool ok = getOK();

  if (ok) {
    size = 1;
  }

  if (ok) {
    return size;
  }

  return 0;
}

int loop1_FP() {

  int size;

  for (;;) {
    size = 1;
    if (getOK())
      break;
  }

  return size;
}

int ok6() {
  int x;
  x = 7;
  return x;
}
