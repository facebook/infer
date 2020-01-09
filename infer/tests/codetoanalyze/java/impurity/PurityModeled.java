/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class PurityModeled {

  double math_random_impure_FN() {
    return Math.random();
  }

  void arraycopy_pure_FP(int[] src) {
    int[] dst = {5, 10, 20, 30, 40, 50};
    // copies an array from the specified source array
    System.arraycopy(src, 0, dst, 0, 1);
  }

  public void array_length_loop_pure(Integer[] array) {
    for (int i = 0; i < array.length; i++) {}
  }
}
