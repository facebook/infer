// Copyright (c) 2015-Present Facebook. All rights reserved.

package codetoanalyze.java.tracing;

import com.facebook.infer.annotation.Verify;

public class ArrayIndexOutOfBoundsExceptionExample {

  void callMethodFromArray(T[] array, int index) {
    if (array[index] != null) {
      array[index].f();
    }
  }

  @Verify
  public void missingCheckOnIndex(T[] array, int index) {
    if (array != null) {
      if (index < array.length) {
        callMethodFromArray(array, index);
      }
    }
  }

  void callOutOfBound() {
    T[] array = new T[42];
    callMethodFromArray(array, -5);
  }

  void withFixedIndex(T[] array) {
    int index = 9;
    callMethodFromArray(array, index);
  }

  void arrayIndexOutOfBoundsInCallee() {
    T[] array = new T[8];
    withFixedIndex(array);
  }

}
