/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class IntegerExample {

  private static void testIntegerEqualsGood() {
    Integer a = new Integer(42);
    Integer b = new Integer(42);
    Integer c = null;

    if (!a.equals(b)) {
      c.intValue();
    }

    if (a != 42) {
      c.intValue();
    }
  }

  private static void testIntegerEqualsBad() {
    Integer a = new Integer(42);
    Integer b = new Integer(42);
    Integer c = null;

    if (a.equals(b)) {
      c.intValue();
    }
  }

  private static void testIntegerEqualsFN() {
    Integer a = new Integer(42);
    Integer b = new Integer(42);
    Integer c = null;

    if (a == b) {
      c.intValue();
    }
  }
}
