/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int globalVarInTopNs;

namespace {

int globalVarInAnonNs;

struct Baz {
  Baz(int x) : fld_(x) {}
  int fld_;
};

} // namespace

namespace Foo {

int globalVarInFoo;

struct Bar : public Baz {
  Bar(int x, int y) : Baz(x), barFld_(y) {}

  static int classVar;

  int f(const Baz& baz) {
  label:
    return baz.fld_ + fld_ + barFld_ + classVar + globalVarInTopNs +
           globalVarInAnonNs + globalVarInFoo;
  }

  static int fooey(int x) { return Bar(x, x + 1).f(Baz(x)); }

 private:
  int barFld_;
};

} // namespace Foo
