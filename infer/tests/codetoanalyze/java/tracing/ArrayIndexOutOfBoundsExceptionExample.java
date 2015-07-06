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
  void callWithWrongIndex(T[] array, int index) {
    if (array != null) {
      if (index >= 0 && index <= array.length) {
        callMethodFromArray(array, index); // No longer found!
      }
    }
  }

  void callOutOfBound() {
    T[] array = new T[42];
    callMethodFromArray(array, -5);
  }

  void callAtIndex(T[] array, int index) {
    array[index].f();
  }

  void withFixedIndex(T[] array) {
    int index = 9;
    callAtIndex(array, index);
  }

  void ArrayIndexOutOfBoundsInCallee() {
    T[] array = new T[8];
    withFixedIndex(array);
  }

}
