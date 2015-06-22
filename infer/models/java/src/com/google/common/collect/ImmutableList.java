/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
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
