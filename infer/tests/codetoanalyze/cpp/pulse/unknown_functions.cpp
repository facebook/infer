/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct X {
  ~X(){};
  void foo();
};

void unknown_init_ptr_by_ref(X** x);
void unknown_no_init_ptr(X* const* x); // cannot init because const

void init_ok() {
  X* p = nullptr;
  unknown_init_ptr_by_ref(&p);
  p->foo();
}

void const_no_init_bad() {
  X* p = nullptr;
  unknown_no_init_ptr(&p);
  p->foo();
}

void unknown_init_value_by_ref(X** x);

void wrap_unknown_init(X** x) { unknown_init_value_by_ref(x); }

void call_unknown_init_interproc_ok() {
  X* p = nullptr;
  wrap_unknown_init(&p);
  p->foo();
}

void unknown_with_pointer_formal(X* x);

void wrap_unknown_no_init(X* x) { unknown_with_pointer_formal(x); }

void call_init_with_pointer_value_bad() {
  X* p = nullptr;
  wrap_unknown_no_init(p);
  p->foo();
}
