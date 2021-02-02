/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class IntegerExample {

  private static void testIntegerEqualsMethodGood() {
    Integer a = new Integer(42);
    Integer b = new Integer(42);
    Integer c = null;

    if (!a.equals(b)) {
      c.intValue();
    }
  }

  private static void testIntegerEqualsMethodBad() {
    Integer a = new Integer(42);
    Integer b = new Integer(42);
    Integer c = null;

    if (a.equals(b)) {
      c.intValue();
    }
  }

  /*
   * Assignments of the form Integer a = n triggers the method valueOf.
   * The valueOf method caches values between -128 and 127 (inclusive).
   */
  private static void FP_testIntegerBuiltInEqualOperatorCachedValuesOk() {
    Integer a = new Integer(42);
    Integer b = 127;
    Integer c = 127;
    Integer d = null;

    if (a != 42) {
      d.intValue();
    }

    if (b != 127) {
      d.intValue();
    }

    /* This is wrong according to the semantics of valueOf.
     * (b==c should hold in this case as 127 is in the cache interval)
     */

    if (b != c) {
      d.intValue();
    }
  }

  /*
   * Assignments of the form Integer a = n triggers the method valueOf.
   * The valueOf method caches values between -128 and 127 (inclusive).
   */
  private static void testIntegerBuiltInEqualOperatorNonCachedValuesBad() {
    Integer a = 128;
    Integer b = 128;
    Integer c = null;

    // This is correct (a!=b should hold in this case as 128 is out of the cached interval)
    if (a != b) {
      c.intValue();
    }
  }

  private static void testIntegerEqualsMethodMaxValueBad() {
    Integer a = new Integer(2147483647);
    Integer b = new Integer(2147483647);
    Integer c = null;

    if (a.equals(b)) {
      c.intValue();
    }
  }

  private static void testIntegerBuiltInEqualOperatorMaxValueOk() {
    Integer a = new Integer(2147483647);
    Integer b = null;

    if (a != 2147483647) {
      b.intValue();
    }
  }
}
