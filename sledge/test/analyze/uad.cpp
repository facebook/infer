/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct B {
  void foo() {}
  ~B() {
    if (!i_) {
      foo();
    }
  }
  int i_;
};

struct A {
  ~A() {
    if (b_) {
      b_->foo();
    }
  }

  B* b_{nullptr};
};

struct S {
  S() { a.b_ = &b; }

  A a;
  B b;
};

int main(int argc, char* argv[]) {
  S s;
  return 0;
}

int _llair_main() { return main(0, nullptr); }
