/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package com.google.common.collect;

public abstract class ImmutableList<E> {

  public static final class Builder<E> {

    void dereference(Object object) {
      object.hashCode();
    }

    public Builder<E> add(E element) {
      dereference(element);
      return new Builder<>();
    }

    public Builder<E> add(E... elements) {
      dereference(elements);
      return new Builder<>();
    }
  }
}
