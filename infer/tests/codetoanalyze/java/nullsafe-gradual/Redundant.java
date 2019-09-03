/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
