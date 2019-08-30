/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Test {

  public static void newMethod(int n) {
    Unchanged.orderN(n);
  }

  public static void complexityDecrease(int n) {
    Unchanged.orderN(n);
  }

  public static void complexityIncrease(int n) {
    Unchanged.orderN(n);
  }

  public static void main(String args[]) {
    complexityDecrease(10);
    complexityIncrease(10);
  }
}
