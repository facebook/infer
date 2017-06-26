/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util;

import java.io.Serializable;

// abstract so we don't have to implement every method of List
public abstract class List<T> extends AbstractList<T> {

  @Override
  public boolean isEmpty() {
    return super.isEmpty();
  }

  @Override
  public void add(int index, T toAdd) {
    super.add(index, toAdd);
  }

  @Override
  public T remove(int index) {
    return super.remove(index);
  }

  @Override
  public boolean remove(Object o) {
    return super.remove(o);
  }

  @Override
  public void clear() {
    super.clear();
  }

}
