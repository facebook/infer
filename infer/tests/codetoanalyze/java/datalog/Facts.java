/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.util.*;

public class Facts {
  public static void main(String args[]) {
    // Alloc fact
    Object obj = new String("Test");
    // Multiple allocations sites on the same line
    ArrayList l = new ArrayList(new ArrayList(new ArrayList()));

    // Cast fact
    String str = (String) obj;
    // Nested casts
    ArrayList l2 = (ArrayList) ((List) obj);

    // Call with void
    testVoid(str);

    // Move fact
    Cl foo = new Cl(str);
    Cl bar = foo;

    // Store and load facts
    foo.x = new String("Test");
    String y = foo.x;

    // ActualArgs and ActualReturn facts
    String ret_var = testArgs(str, y);
  }

  // FormalArg and FormalReturn facts
  private static String testArgs(String a, String b) {
    String a_loc = a;
    return a_loc;
  }

  // Test void return
  private static void testVoid(String a) {}
}

class Cl {
  public String x;

  // Test non-default constructor (will still appear as "<init>()")
  public Cl(String s) {
    x = s;
  }
}
