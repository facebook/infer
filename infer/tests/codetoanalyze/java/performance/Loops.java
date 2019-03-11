/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

public class Loops {

  static int do_while_independent_of_p(int p) {
    int a = 0;
    do {
      if (p == 15) {
        p = p + 1;
      }
      a++;
    } while (a < 25);

    return 0;
  }

  /* can't handle nested loops yet, but control vars of both loops must
  be {a, b} */
  static void nested_do_while_FP(int p) {
    int a = 10;
    int b = 0;
    do {
      do {
        if (p == 15) {
          p = p + 1;
        }
        b++;
      } while (b < 10);
      a++;
    } while (a < 20);
  }

  static void dumb0(long[] a, int length) {
    for (int i = 1; i < length; i++) {
      if (a[i] < a[i - 1]) {
        a[i - 1] = 0;
      } else {
        a[i] = 1;
      }
    }
  }

  static void dumbSort(long[] a, long[] b, int length) {
    for (int i = length - 1; i >= 0; --i) {
      for (int j = 0; j < i; ++j) {
        if (a[j] * b[j + 1] > a[j + 1] * b[j]) {
          long temp = a[j + 1];
          a[j + 1] = a[j];
          a[j] = temp;
          temp = b[j + 1];
          b[j + 1] = b[j];
          b[j] = temp;
        }
      }
    }
  }

  public static class C {
    public char c;
    public float[] f;
  }

  static boolean similar(C[] x, C[] y) {
    if (x == null || y == null || x.length != y.length) {
      return false;
    }
    for (int i = 0; i < x.length; i++) {
      if (x[i].c != y[i].c || x[i].f.length != y[i].f.length) {
        return false;
      }
    }
    return true;
  }

  static void linear(int x) {
    for (int i = 0; i < x; i++) {}
  }

  static void unboundedSymbol() {
    int infinite = 9;
    for (int i = 0; i < 999; i++) {
      infinite *= infinite;
    }
    linear(infinite);
  }
}
