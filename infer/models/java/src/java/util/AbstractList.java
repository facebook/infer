/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util;

import com.facebook.infer.builtins.InferUndefined;

// could make List an abstract class directly instead, but that breaks other models
public abstract class AbstractList<T> extends AbstractCollection<T> implements List<T> {

  // need three-value state for unknown (-1), false (0), or true (1)
  // the reason we can't use a bool is that we want to return either true or false each time we call
  // isEmpty, and we want to get the same result each time
  private int mIsEmpty;

  native T any();

  public AbstractList() {
    mIsEmpty = 1;
  }

  @Override
  public boolean isEmpty() {
    if (mIsEmpty < 0) {
      if (InferUndefined.boolean_undefined()) {
        mIsEmpty = 1;
      } else {
        mIsEmpty = 0;
      }
    }
    return mIsEmpty == 1;
  }

  @Override
  public void add(int index, T toAdd) {
    mIsEmpty = 0;
  }

  @Override
  public T remove(int index) {
    mIsEmpty = -1;
    return any();
  }

  @Override
  public boolean remove(Object o) {
    boolean result = InferUndefined.boolean_undefined();
    if (result) {
      mIsEmpty = -1;
    }
    return result;
  }

  @Override
  public void clear() {
    mIsEmpty = 1;
  }

}
