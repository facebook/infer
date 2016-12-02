/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.util.HashMap;

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
