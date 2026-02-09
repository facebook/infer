/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;


class A {
  static void source() {
    B.source();
  }
}

class B {
  static void source() {
    C.source();
  }
}

class C {
  static void source() {
    sink();
  }

  static void sink() {}
}
