/*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.performance;

public class Cost_test {

  // Cost: 5
  private static int foo_OK() {
    int i, j;
    i = 17;
    j = 31;

    return i + j + 3 + 7;
  }

  // Cost: 17
  private static int bar_OK() {

    int j = 0;

    j++;
    j++;
    j++;
    j = foo_OK();
    j++;

    return j;
  }

  // Cost: 25
  private static int cond_OK(int i) {
    int x;

    if (i < 0) {
      x = bar_OK();
    } else {
      x = 1;
    }
    return x;
  }

  // Cost: 5
  private static void alias_OK() {

    int i = 0, j;

    j = i;
    i = ++i;
  }

  // Cost: 7
  private static void alias2_OK() {

    int i = 0, j, z;

    j = 1;
    z = 2;

    j = i;
    i = z;
  }

  // Cost: 1101
  private static int loop0_bad() {

    for (int i = 0; i < 100; i++) {
      alias2_OK();
    }
    return 0;
  }

  // Cost: 1203
  private static int loop1_bad() {

    int k = 100;
    for (int i = 0; i < k; i++) {
      alias2_OK();
    }
    return 0;
  }

  // Expected: Linear bound
  private static int FN_loop2(int k) {

    for (int i = 0; i < k; i++) {
      alias2_OK();
    }
    return 0;
  }

  // Expected: constant
  private static int loop3(int k) {

    for (int i = k; i < k + 18; i++) {
      alias2_OK();
    }
    return 0;
  }

  // Cost: 218
  // Shows that calling many times non expensive function can
  // result in an expensive computation
  private static int main_bad() {

    int k1, k2, k3, k4;

    cond_OK(2);
    k1 = bar_OK() + foo_OK() + cond_OK(15) * 2;
    k2 = bar_OK() + foo_OK() + cond_OK(17) * 3;
    k3 = bar_OK() + foo_OK() + cond_OK(11) * 3;
    k4 = bar_OK() + foo_OK() + cond_OK(19) * 3;
    return 0;
  }
}
