/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
public class Redundant {

  void bad() {
    String s = "123";
    if (s != null) {
      int n = s.length();
    }
  }

  static void maythrow() throws java.io.IOException {}

  void good() throws java.io.IOException {
    String s = null;

    try {
      maythrow();
      s = "123";
    } finally {
      if (s != null) { // this is not redundant
        int n = s.length();
      }
    }
  }
}
