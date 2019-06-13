/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Arrays {
  static ArraysB[] arrayB = new ArraysB[10];

  static ArraysC[] arrayC = ArraysD.arrayC;

  public static void main(String args[]) {
    ArraysA[] arrayA = new ArraysA[10];

    System.out.println(ArraysF.foo()[0]);

    ArraysG[][] arrayG = new ArraysG[10][10];
  }
}

class ArraysA {}

class ArraysB {}

class ArraysC {}

class ArraysD {
  static ArraysC[] arrayC = new ArraysC[10];
}

class ArraysE {}

class ArraysF {
  static ArraysE[] foo() {
    return new ArraysE[10];
  }
}

class ArraysG {}
