/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

public class ArrayOutOfBounds {

  public int arrayOutOfBounds() {
    int[] arr = new int[1];
    return arr[3];
  }

  public int arrayInBounds() {
    int[] arr = new int[2];
    return arr[1];
  }

  // tests below this line are turned off until array functionality improves
  public void arrayLoopOutOfBounds(int[] arr) {
    for (int i = 0; i <= arr.length; i++) {
      int j = arr[i];
    }
  }

  public void arrayLoopInBounds(int[] arr) {
    for (int i = 0; i < arr.length; i++) {
      int j = arr[i];
    }
  }

  public void buggyIter(int[] arr1, int[] arr2) {
    for (int i = 0; i < arr1.length; i++) {
      arr2[i] = 7;
    }
  }

  public void switchedArrsOutOfBounds() {
    buggyIter(new int[11], new int[10]);
  }

  public void buggyNestedLoop1(int[] arr1, int[] arr2) {
    for (int i = 0; i < arr1.length; i++) {
      for (int j = 0; i < arr2.length; j++) {
        arr1[i] = arr1[i] + arr2[j];
      }
    }
  }

  public void nestedOutOfBounds1() {
    buggyNestedLoop1(new int[11], new int[10]);
  }

  public void buggyNestedLoop2(int[] arr1, int[] arr2) {
    for (int i = 0; i < arr1.length; i++) {
      for (int j = 0; j < arr2.length; i++) {
        arr1[i] = arr1[i] + arr2[j];
      }
    }
  }

  public void nestedOutOfBounds2() {
    buggyNestedLoop2(new int[11], new int[10]);
  }

  public void buggyNestedLoop3(int[] arr1, int[] arr2) {
    for (int i = 0; i < arr1.length; i++) {
      for (int j = 0; j < arr2.length; j++) {
        arr1[i] = 2 * arr2[i];
      }
    }
  }

  public void nestedOutOfBounds3() {
    buggyNestedLoop3(new int[11], new int[10]);
  }

  public void safeNestedLoop(int[] arr1, int[] arr2) {
    for (int i = 0; i < arr1.length; i++) {
      for (int j = 0; j < arr2.length; j++) {
        arr1[i] = arr1[i] + arr2[j];
      }
    }
  }

  public void nestedInBounds() {
    safeNestedLoop(new int[11], new int[10]);
  }
}
