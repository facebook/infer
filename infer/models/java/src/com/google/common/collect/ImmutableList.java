/*
* Copyright (c) 2013 - present Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD style license found in the
* LICENSE file in the root directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*/

package com.google.common.collect;

import com.facebook.infer.models.InferBuiltins;

public class ImmutableList<E> {

  private static final ImmutableList<Object> EMPTY = new ImmutableList<Object>();

  @SuppressWarnings("unchecked")
  public static <E> ImmutableList<E> of() {
    InferBuiltins.assume(EMPTY != null);
    return (ImmutableList<E>) EMPTY;
  }

}
