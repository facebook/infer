/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class A {}

class B {}

class LongBadName {}

public class TypeFilter {
  static void f(Object x) {}

  static void aOk(A x) {
    f(x);
  }

  static void bBad(B x) {
    f(x);
  }

  static void cBad(LongBadName x) {
    f(x);
  }

  static void dBad() {
    f(new B());
  }
}
