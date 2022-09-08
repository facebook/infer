/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

class E extends Exception {
  E(int data) {
    this.data = data;
  }

  int data;
}

public class IntraExceptions {

  // intra-exceptions #1
  /*
    try block conditionally throws an exception that is caught by the
    catch block.
  */
  private static int intra_exceptions_1(int x) {
    try {
      int y = x + 10;
      if (y < 0) throw new E(y);
      return 10;
    } catch (E e) {
      return e.data;
    }
  }

  private static void testIntraExceptions1Bad() {
    Integer my_null_int = null;
    if (intra_exceptions_1(-100) == -90) {
      my_null_int.intValue();
    }
  }

  private static void testIntraExceptions1Good() {
    Integer my_null_int = null;
    if (intra_exceptions_1(-100) == 0) {
      my_null_int.intValue();
    }
  }

  // intra-exceptions #2
  /*
    try block unconditionally throws an exception that is caught by the catch block.
  */
  private static int intra_exceptions_2(int x) {
    try {
      throw new E(x + 22);
    } catch (E e) {
      return e.data;
    }
  }

  private static void testIntraExceptions2Bad() {
    Integer my_null_int = null;
    if (intra_exceptions_2(10) == 32) {
      my_null_int.intValue();
    }
  }

  private static void testIntraExceptions2Good() {
    Integer my_null_int = null;
    if (intra_exceptions_2(10) == 0) {
      my_null_int.intValue();
    }
  }
}
