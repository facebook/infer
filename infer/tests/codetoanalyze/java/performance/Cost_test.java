/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

import java.io.IOException;
import java.io.InputStream;

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

  // Cost: 1
  private static void unitCostFunction() {}

  boolean rand() {
    if (Math.random() > 0.5) {
      return true;
    } else {
      return false;
    }
  }

  // Cost: Linear to n, not b
  void ignore_boolean_symbols_linear(boolean b, int n) {
    for (int i = 0; b && i < n; i++) {
      b = true;
    }
  }

  // Cost should not include the symbol of b.
  void ignore_boolean_symbols_constant1(boolean b) {
    for (; b; ) {
      if (rand()) {
        b = true;
      }
    }
  }

  // Cost should not include the symbol of b.
  void ignore_boolean_symbols_constant2(boolean b) {
    for (; b; ) {
      if (rand()) {
        b = false;
      }
    }
  }

  // Cost should not include the symbol of f.
  void ignore_float_symbols_constant(float f) {
    for (; f < (float) 1.0; ) {
      if (rand()) {
        f = (float) 1.0;
      }
    }
  }

  // Cost should not include the symbol of d.
  void ignore_double_symbols_constant(double d) {
    for (; d < (double) 1.0; ) {
      if (rand()) {
        d = 1.0;
      }
    }
  }

  // Cost should not include the symbol of c.
  void ignore_character_symbols_constant(char c) {
    for (; c < 'z'; ) {
      if (rand()) {
        c = 'a';
      }
    }
  }

  void call_inputstream_read_linear(InputStream is) throws IOException {
    int total = 0;
    int r;
    byte[] buf = new byte[20];
    while (total < 100 && (r = is.read(buf, 0, 20)) != -1) {
      total += r;
    }
  }

  static int global;

  int get_global() {
    return global;
  }

  /* It instantiates the return value of `get_global` (= `global`, the value of which is unknown) to
  the `global` symbol, instead of top, in order to avoid useless top-cost results.  */
  void loop_on_unknown_global_linear() {
    for (int i = 0; i < get_global(); i++) {}
  }

  void band_constant(int x) {
    for (int i = 0; i < (int) (x & 0xff); i++) {}
  }
}

class CloneTest {
  int i;

  void clone_test_constant() throws CloneNotSupportedException {
    this.i = 10;
    CloneTest o = (CloneTest) this.clone();
    for (int i = 0; i < o.i; i++) {}
  }
}
