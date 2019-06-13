/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package java.util;

// abstract so we don't have to implement every method of List
public abstract class ArrayList<T> extends AbstractList<T> {

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
