/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
namespace {
struct Foo {
  void bar(){};
  void toUnsafeFuture(){};
};
struct Bar {
  virtual void fooBar() { f.bar(); };
  Foo f;
};
} // anonymous namespace

int main() {
  Foo{}.bar();
  Foo f;
  f.bar();
  Bar{}.fooBar();
  return 0;
}
