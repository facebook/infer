/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

public class Compound_loop {

  /* while loop that contains && in the guard. It gives the correct bound  */
  private static int compound_while(int m) {
    int i = 0;
    int j = 3 * i;
    while (j == 0 && i < m) {
      i++;
    }
    return j;
  }

  /* p should be in control vars */
  private static void while_and_or(int p) {
    int i = 0;
    while (p == 1 || (i < 30 && i >= 0)) {
      i++;
    }
  }

  // should be constant cost
  int nested_while_and_or(int p) {
    int i = 0;
    int j = 3 * i;
    while (p == 1 || (i < 30 && i >= 0)) {
      while (p == 1 || (j < 5 && j >= 0)) {

        return j;
      }
      i++;
    }
    return j;
  }
}
