/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class ImmutableArray {

  final int[] testArray = new int[] {0, 1, 2, 4};

  int[] getTestArray() {
    return testArray;
  }

  void badA() {
    int[] array = getTestArray();
    array[2] = 7;
  }

  void badB() {
    int[] array = getTestArray();
    int[] otherArray = array;
    otherArray[2] = 7;
  }

  void badC() {
    int[] array = getTestArray();
    otherMutateArray(array);
  }

  void badABC() {
    int[] array = getTestArray();
    array[2] = 7;
    int[] otherArray = array;
    otherArray[2] = 7;
    otherMutateArray(array);
  }

  void otherMutateArray(int[] array) {
    array[2] = 7;
  }
}
