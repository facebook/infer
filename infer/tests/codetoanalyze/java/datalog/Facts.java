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
  }

  // FormalArg and FormalReturn facts
  private static String testArgs(String a, String b) {
    String a_loc = a;
    return a_loc;
  }

  // Test void return
  private static void testVoid(String a) {}
}
