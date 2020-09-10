/*
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class Foo {
 public:
  void func() {} // a non-const member function
};

void someFunction(const Foo &f) {
  Foo &fRef = const_cast<Foo &>(f);
  fRef.func(); // okay
}

constexpr int i = 1;
