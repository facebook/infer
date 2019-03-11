/*
 * Copyright (c) 2019-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int globalVarInTopNs;

namespace {

int globalVarInAnonNs;

struct Baz {
  int fld_;
};

} // namespace

namespace Foo {

int globalVarInFoo;

class Bar : public Baz {
  static int classVar;
  int f(const Baz& baz) {
  label:
    return baz.fld_ + fld_ + barFld_ + classVar + globalVarInTopNs +
           globalVarInAnonNs + globalVarInFoo;
  }

 private:
  int barFld_;
};

} // namespace Foo
