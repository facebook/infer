/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace interproc {

struct is {
  int n;
  int b;
};

void i_init(int* i) { *i = 10; }

void i_no_init(int* i) { i = 0; }

// error is detected before call if x was not init
int i_inc(int x) { return x++; }

int no_init_in_callee_bad_FN() {
  int a;
  int b = 0;

  i_no_init(&a);

  b = a; // error
  return b;
}

int init_in_callee_ok() {
  int a;
  int b = 0;

  i_init(&a);
  b = a; // OK
  return b;
}

int no_init_field_in_callee_bad_FN() {
  struct is t;
  int b = 0;

  i_no_init(&t.n);

  b = t.n; // error
  return b;
}

int init_field_in_callee_ok() {
  struct is t;
  int b = 0;

  i_init(&t.n);

  b = t.n; // OK
  return b;
}

int no_init_in_callee_bad2_FN() {
  int a;
  int c = 0;

  i_no_init(&a);

  c = i_inc(a); // error
  return c;
}

int init_in_callee_ok2() {
  int a;
  int c = 0;

  i_init(&a);

  c = i_inc(a); // ok
}

int i_no_init_return_bad() {
  int x;
  return x; // error
}

// this shows that in case a function return an uninit value, it gets the
// blame / rather than the caller.
int blame_on_callee() {
  int a;
  int c = i_no_init_return_bad();

  a = c; // we don't flag the error here as it is flagged in no_init_return
  // definition

  return 0;
}

void i_maybe_init(int y, int* formal) {

  if (y == 0) {
    *formal = 5;
  };
}

void i_must_init_ok(int y, int* formal) {

  if (y == 0) {
    *formal = 5;
  } else {
    *formal = 17;
  };
}

int i_call_maybe_init_bad_FN(int y) {
  int x;
  i_maybe_init(y, &x);
  return x;
}

int i_call_must_init_ok(int y) {
  int x;
  i_must_init_ok(y, &x);
  return x;
}

void i_square_init(int x, int& res) { res = x * x; }

int i_square_no_init(int x, int& res) { return res * res; }

void i_use_square_OK() {

  int i;
  i_square_init(2, i);
}

void FN_use_square_bad() {

  int i;
  i = i_square_no_init(2, i); // We should report here
}

int i_no_deref(int* x) {
  int* y = 0;
  x = y;
  return *x; // this is not actually a deref of a formal
}

void i_init_x(int* x) {
  int* y;
  y = x;
  *y = 25; // this is writing x. Need aliasing info
}

int FP_use_init_x_OK() {

  int a;
  i_init_x(&a);

  return a; // We should not report here
}

struct s {
  int n;
  int b;
};

void init_some_field_of_struct(struct s* z) { z->n = 10; };

void init_full_struct(struct s* z) {
  z->n = 10;
  z->b = 17;
};

int call_init_some_field_of_struct_ok() {
  struct s t;
  init_some_field_of_struct(&t);

  return t.n;
}

int call_init_some_field_of_struct_bad() {
  struct s t;
  init_some_field_of_struct(&t);

  return t.b;
}

int call_init_full_struct_ok() {
  struct s t;
  init_full_struct(&t);

  int i = t.n;
  int b = t.b;

  return 0;
}
} // namespace interproc
