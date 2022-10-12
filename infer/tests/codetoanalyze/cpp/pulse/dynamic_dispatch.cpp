/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace dynamic_dispatch {

struct Base {
  virtual int foo() { return 32; }
};

struct Derived : Base {
  int foo() { return 52; }
};

Base getDerived() { return Derived{}; }

Base* getDerivedPtr() { return new Derived(); }

void dispatch_to_Base_bad() {
  Base base{};
  if (base.foo() == 32) {
    int* p = nullptr;
    *p = 42;
  }
}

// FN because we do not track dynamic types unless it's a heap allocation
// initiated by new()
void FN_dispatch_to_Derived_bad() {
  Base derived = getDerived();
  if (derived.foo() == 52) {
    int* p = nullptr;
    *p = 42;
  }
}

void dispatch_to_Derived_ptr_bad() {
  Base* derived = getDerivedPtr();
  if (derived->foo() == 52) {
    int* p = nullptr;
    *p = 42;
  }
}

} // namespace dynamic_dispatch
