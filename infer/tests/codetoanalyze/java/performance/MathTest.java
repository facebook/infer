/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.java.performance;

class MathTest {

  void min_constant(int arr[]) {
    for (int i = 0; i < Math.min(3, arr.length); i++) {}
  }

  void max_symbolic(int arr[]) {
    for (int i = 0; i < Math.max(0, arr.length); i++) {}
  }

  void max2_symbolic(int x, int y) {
    for (int i = 0; i < Math.max(x, y); i++) {}
  }

  void call_max2_constant() {
    max2_symbolic(10, 20);
  }

  void linear(int p) {
    for (int count = 0; count < p; count++) {}
  }

  void call_with_min_constant() {
    linear(Math.min(3, 10));
  }

  void call_with_max_linear(int x) {
    linear(Math.max(1, x));
  }
}
