/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.google.common.collect;

import com.facebook.infer.builtins.InferBuiltins;
import java.util.NoSuchElementException;
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
