/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util;

import com.facebook.infer.builtins.InferUndefined;

public abstract class Vector<E> {

  protected E[] elementData;

  E elementData(int index) {
    return (E) elementData[index];
  }

  public Enumeration<E> elements() {
    return new Enumeration<E>() {
      int count;

      public boolean hasMoreElements() {
        return count > 0;
      }

      public E nextElement() {
        if (count > 0) return (E) InferUndefined.object_undefined();
        else throw new NoSuchElementException();
      }
    };
  }

  public E get(int index) {
    return elementData(index);
  }

  public int size() {
    return InferUndefined.nonneg_int();
  }
}
