/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
void assign_zero_ok() {
  int x[2];
  x[1] = 42;
}

void deref_nullptr_bad() {
  int* p = nullptr;
  *p = 42;
}

void guarded_nullptr_ok() {
  int* p = nullptr;
  if (p != nullptr) {
    *p = 42;
  }
}

struct X {
  void foo();
};

bool choice();

X* may_return_nullptr() {
  if (choice) {
    return nullptr;
  }
  return new X();
}

void no_check_return_bad() {
  X* x = may_return_nullptr();
  x->foo();
}

void check_return_ok() {
  X* x = may_return_nullptr();
  if (x != nullptr) {
    x->foo();
  }
}

void compare_to_null(void* x) {
  if (x) {
  }
}

void deref_after_compare_ok(int* x) {
  compare_to_null(x);
  *x = 42;
}
