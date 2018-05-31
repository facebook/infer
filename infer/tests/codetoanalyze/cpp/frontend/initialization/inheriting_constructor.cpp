/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
struct A {
  A(int) {}
  void foo(){};
};

struct B : A {
  using A::A;
};

int main() { B b(5); }
