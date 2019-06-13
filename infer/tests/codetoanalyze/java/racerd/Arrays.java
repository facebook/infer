/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import javax.annotation.concurrent.ThreadSafe;

// Test may_alias treatment of arrays
// two arrays of types in a subtype relation may alias, and race

@ThreadSafe
class Parent {}

@ThreadSafe
class Child extends Parent {}

@ThreadSafe
class Arrays {
  Child[] childArr = new Child[5];
  Parent[] parentArr = childArr; // actual aliasing not required, but for documentation
  final String[] strArr1 = new String[5];
  final String[] strArr2 = new String[5];

  void arrayParameterWriteBad(int[] name1) {
    name1[2] = 4;
  }

  // although name1 and name2 may alias, we have no reason to think that they will. don't report
  int FN_arrayParameterReadBad(int[] name2) {
    return name2[2];
  }

  int arrayParameterLiteralReadOk() {
    return (new int[] {2, 3})[1];
  }

  public void writeWriteRaceBad(String s) {
    strArr1[2] = s;
  }

  // same array
  public String readWriteRaceBad(String s) {
    synchronized (this) {
      strArr1[2] = s;
    }
    return strArr1[2];
  }

  // arrays are same type, but can't alias
  public String notReadWriteRace1Ok(String s) {
    synchronized (this) {
      strArr1[0] = s;
    }
    return strArr2[0];
  }

  // arrays are compatible types and can alias
  public Child FN_readWriteAliasRaceBad() {
    synchronized (this) {
      parentArr[3] = null;
    }
    return childArr[3];
  }

  String[] type1Arr[];
  Parent[] type2Arr;

  // arrays are different types and thus cannot alias
  public Parent noRaceOk() {
    synchronized (this) {
      type1Arr[3] = null;
    }

    return type2Arr[3];
  }
}
