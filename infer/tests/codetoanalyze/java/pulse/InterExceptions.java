/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

class D extends Exception {
  D(int data) {
    this.data = data;
  }

  int data;
}

public class InterExceptions {

  /*
      g conditionally throws an exception, which is caught by the caller.
  */
  private static int g(int x) throws D {
    if (x < 0) {
      throw new D(x);
    }
    return x + 22;
  }

  /*
    caller_of_g(x) ==
      x - 10  if x < 10
      x + 12  if x >= 10
  */
  private static int caller_of_g(int x) {
    try {
      return g(x - 10);
    } catch (D d) {
      return d.data;
    }
  }

  private static void testInterExceptions1Bad() {
    Integer my_null_int = null;
    if (caller_of_g(100) == 112) {
      my_null_int.intValue();
    }
  }

  private static void testInterExceptions1Good() {
    Integer my_null_int = null;
    if (caller_of_g(100) != 112) {
      my_null_int.intValue();
    }
  }

  private static void testInterExceptions2Bad() {
    Integer my_null_int = null;
    if (caller_of_g(0) == -10) {
      my_null_int.intValue();
    }
  }

  private static void testInterExceptions2Good() {
    Integer my_null_int = null;
    if (caller_of_g(0) != -10) {
      my_null_int.intValue();
    }
  }

  /*
      thrower unconditionally throws an exception, which is caught by the caller.
  */
  private static int thrower(int x) throws D {
    throw new D(x);
  }

  /*
    caller_of_thrower(x) == x + 23
  */
  private static int caller_of_thrower(int x) {
    try {
      return thrower(x + 23);
    } catch (D d) {
      return d.data;
    }
  }

  private static void testInterExceptions3Bad() {
    Integer my_null_int = null;
    if (caller_of_thrower(10) == 33) {
      my_null_int.intValue();
    }
  }

  private static void testInterExceptions3Good() {
    Integer my_null_int = null;
    if (caller_of_thrower(10) != 33) {
      my_null_int.intValue();
    }
  }
}
