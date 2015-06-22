/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package com.google.common.collect;

import java.util.NoSuchElementException;

import com.facebook.infer.models.InferBuiltins;

import javax.annotation.Nullable;

public class Iterators {

  public static <T> UnmodifiableIterator<T> singletonIterator(@Nullable final T value) {
    return new UnmodifiableIterator<T>() {
      boolean done;

      @Override
      public boolean hasNext() {
        return !done;
      }

      @Override
      public T next() {
        InferBuiltins.assume(value != null);
        if (done) {
          throw new NoSuchElementException();
        }
        done = true;
        return value;
      }
    };
  }

}
