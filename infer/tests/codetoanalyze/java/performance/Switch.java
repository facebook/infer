/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

public class Switch {
  // Cost 51
  private static void vanilla_switch(int i) {

    for (int p = 0; p < 100; p++) {
      switch (p) {
        case 0:
          i++;
          break;
        case 1:
        case 2:
        case 3:
          break;
        default:
          return;
      }
    }
  }
  // 797
  private static int test_switch() {
    int value = 0;
    // infinite loop
    while (value < 100) {
      switch (value) {
        case 0:
          break;
        case 1:
          continue;
        case 2:
        default:
          continue;
      }
      value++;
    }
    return 0;
  }
}
