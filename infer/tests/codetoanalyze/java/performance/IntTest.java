/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class IntTest {
  void intValue_linear(Integer mKBytesToSend) {
    for (int count = 0; count < mKBytesToSend; count++) {}
  }

  static Integer static_Integer;

  static void static_Integer_top() {
    for (int count = 0; count < static_Integer; count++) {}
  }

  static void valueOf_linear(int p) {
    Integer x = p; // call to valueOf
    for (int count = 0; count < x; count++) {}
  }

  boolean unknown_bool;

  void control_var_band_constant(int x, int y) {
    int z;
    for (int i = 0; i < 10; i++) {
      if (unknown_bool) {
        z = x & y;
      } else {
        z = x | y;
      }
      if (z == 0) {
        break;
      }
    }
  }

  void control_var_band_add_constant(int x, int y) {
    int z;
    for (int i = 0; i < 10; i++) {
      if (unknown_bool) {
        z = (x & y) + 1;
      } else {
        z = (x | y) + 1;
      }
      if (z == 0) {
        break;
      }
    }
  }
}
