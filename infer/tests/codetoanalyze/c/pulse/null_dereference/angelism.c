/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>

struct delicious {
  int yum;
  int* ptr;
};

struct delicious* bake(struct delicious** cake) {
  int* zero = NULL;
  *zero = 3;
  return NULL;
}

struct delicious* skip_function_with_no_spec(void) {
  struct delicious* cake = NULL;
  int i;

  if (bake(&cake) == NULL) {
    return 0;
  }

  i = cake->yum;
  return cake;
}

extern struct delicious* bakery(struct delicious** cake);
extern struct delicious* bakery2(struct delicious** cake,
                                 struct delicious** pie);
extern struct delicious* returnPassByRef();
extern void skip(struct delicious* s);

struct delicious* skip_external_function(void) {
  struct delicious* cake = NULL;
  int i;

  if (bakery(&cake) == NULL) {
    return 0;
  }

  i = cake->yum;
  return cake;
}

void by_ref_actual_already_in_footprint(struct delicious* param) {
  int i;
  struct delicious* ret = bakery(&param);
  i = param->yum;
}

void call_by_ref_actual_already_in_footprint_ok() {
  by_ref_actual_already_in_footprint(NULL); // should not report a warning
}

void by_ref_actual_already_in_footprint2(struct delicious* param) {
  int i;
  i = param->yum; // should not report a warning
  struct delicious* ret = bakery(&param);
  i = param->yum; // should not report a warning
}

void call_by_ref_actual_already_in_footprint_bad() {
  by_ref_actual_already_in_footprint2(NULL); // should report a warning
}

void passByRefTwice() {
  struct delicious* param;
  bakery2(&param, &param); // should not report a warning
  int i = param->yum;
}

struct delicious* returnPassByRef2() {
  struct delicious* param = NULL;
  bakery(&param);
  int i = param->yum; // should not report a warning
  return param;
}

void returnPassByRefDeref() {
  struct delicious* ret = returnPassByRef();
  ret->yum = 2; // should not report a warning
  free(ret);
}

extern void struct_ptr_skip(struct delicious* s);

extern void struct_val_skip(struct delicious s);

int passStructByRefDeref() {
  struct delicious d;
  d.yum = 7;
  struct_ptr_skip(&d);
  return 1 / d.yum; // should not report divide by zero warning
}

int struct_value_by_ref_pure() {
  struct delicious x;
  struct_ptr_skip(&x);
  return 1 / x.yum; // should not report divide by zero warning
}

int struct_value_by_ref_ptr() {
  struct delicious x;
  struct_ptr_skip(&x);
  return *x.ptr; // should not report null deref warning
}

int struct_value_by_ref_abduce() {
  struct delicious x;
  struct_ptr_skip(&x);
  return 1 / *x.ptr; // shoult not report divide by zero warning
}

int struct_value_by_ref_ptr_write_before() {
  struct delicious x;
  x.ptr = NULL;
  struct_ptr_skip(&x);
  return *x.ptr; // should not report null deref warning
}

int struct_value_by_ref_ptr_write() {
  struct delicious x;
  struct_ptr_skip(&x);
  x.ptr = NULL;
  return *x.ptr; // should report null deref warning
}

void setF(struct delicious* x, int val) { x->ptr = val; }

int struct_value_by_ref_callee_write_no_skip() {
  struct delicious x;
  setF(&x, NULL);
  return *x.ptr; // should report null deref warning
}

int struct_value_by_ref_callee_write_skip() {
  struct delicious x;
  struct_ptr_skip(&x);
  setF(&x, NULL);
  return *x.ptr; // should report null deref warning
}

int struct_value_by_ref_write_then_skip() {
  struct delicious x;
  x.ptr = NULL;
  struct_ptr_skip(&x);
  return *x.ptr; // should not report null deref warning
}

int struct_value_skip_null_deref() {
  struct delicious x;
  x.ptr = NULL;
  struct_val_skip(x);
  return *x.ptr; // should report null deref warning
}

int struct_value_skip_ok() {
  struct delicious x;
  x.yum = 7;
  struct_val_skip(x);
  return 1 / x.yum; // should not report div by zero warning
}

int struct_value_from_pointer_skip_ok(struct delicious* x) {
  struct_val_skip(*x);
  return 1 / x->yum; // should not report div by zero warning
}

int struct_value_from_pointer_skip_bad(struct delicious* x) {
  x->ptr = NULL;
  struct_val_skip(*x);
  return 1 / *x->ptr; // should report null deref warning
}
