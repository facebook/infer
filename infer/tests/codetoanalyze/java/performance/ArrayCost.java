/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
public class ArrayCost {

  // expected: Theta(mag.length)
  private void ArrayCost(int[] mag) {

    int i = 0;
    int k = mag.length;

    while (i < k) {
      i++;
    }
  }

  // expected: ~31
  private static boolean isPowOfTwo_FP(int value) {
    int ones = 0;
    int v = value;

    for (int shifts = 0; shifts < 31 && ones <= 1; shifts++) {
      if ((v & 1) == 1) {
        ones++;
      }

      v >>= 1;
    }

    return ones == 1;
  }
}
