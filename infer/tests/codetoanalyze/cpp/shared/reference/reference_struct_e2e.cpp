/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct X {
  int f;
  void nonzero() { f = 1; }
  void zero() { f = 0; }
  int div() { return 1 / f; }
};

void zero_ptr(X* x) { x->zero(); }

void nonzero_ptr(X* x) { x->nonzero(); }

void set_field_ptr(X* x, int val) { x->f = val; }

void zero_ref(X& x) { x.zero(); }

void nonzero_ref(X& x) { x.nonzero(); }

void set_field_ref(X& x, int val) { x.f = val; }

X global;
X* get_global_ptr() { return &global; }
X& get_global_ref() { return global; }

int method_div0_ptr(X* x) {
  if (x) {
    zero_ptr(x);
    return x->div();
  }
}

int method_div1_ptr(X* x) {
  if (x) {
    nonzero_ptr(x);
    return x->div();
  }
}

int field_div0_ptr(X* x) {
  if (x) {
    set_field_ptr(x, 0);
    return x->div();
  }
}

int field_div1_ptr(X* x) {
  if (x) {
    set_field_ptr(x, 1);
    return x->div();
  }
}

int get_global_ptr_div0_method() {
  get_global_ptr()->f = 1;
  get_global_ptr()->zero();
  get_global_ptr()->div();
}

int get_global_ptr_div1_method() {
  get_global_ptr()->f = 0;
  get_global_ptr()->nonzero();
  get_global_ptr()->div();
}

int get_global_ptr_div0_field() {
  get_global_ptr()->nonzero();
  get_global_ptr()->f = 0;
  get_global_ptr()->div();
}

int get_global_ptr_div1_field() {
  get_global_ptr()->zero();
  get_global_ptr()->f = 1;
  get_global_ptr()->div();
}

int method_div0_ref(X& x) {
  zero_ref(x);
  return x.div();
}

int method_div1_ref(X& x) {
  nonzero_ref(x);
  return x.div();
}

int field_div0_ref(X& x) {
  set_field_ref(x, 0);
  return x.div();
}

int field_div1_ref(X& x) {
  set_field_ref(x, 1);
  return x.div();
}

int get_global_ref_div0_method() {
  get_global_ref().f = 1;
  get_global_ref().zero();
  get_global_ref().div();
}

int get_global_ref_div1_method() {
  get_global_ref().f = 0;
  get_global_ref().nonzero();
  get_global_ref().div();
}

int get_global_ref_div0_field() {
  get_global_ref().nonzero();
  get_global_ref().f = 0;
  get_global_ref().div();
}

int get_global_ref_div1_field() {
  get_global_ref().zero();
  get_global_ref().f = 1;
  get_global_ref().div();
}
