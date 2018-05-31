/*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.tracing;

import com.facebook.infer.annotation.Verify;

public class ArrayIndexOutOfBoundsExceptionExample {

  void callMethodFromArray(T2[] array, int index) {
    if (array[index] != null) {
      array[index].f();
    }
  }

  @Verify
  public void missingCheckOnIndex(T2[] array, int index) {
    if (array != null) {
      if (index < array.length) {
        callMethodFromArray(array, index);
      }
    }
  }

  void callOutOfBound() {
    T2[] array = new T2[42];
    callMethodFromArray(array, -5);
  }

  void withFixedIndex(T2[] array) {
    int index = 9;
    callMethodFromArray(array, index);
  }

  void arrayIndexOutOfBoundsInCallee() {
    T2[] array = new T2[8];
    withFixedIndex(array);
  }

}
