/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package genrule.module1;

import genrule.annotations.Nullable;

public abstract class Class1 {

  public @Nullable Object field1;

  public int x;
  public @Nullable Class1 field2;

  public static @Nullable String returnsNull() {
    return null;
  }

  void localNPE1() {
    Object obj = null;
    obj.toString();
  }

  public abstract @Nullable Object abstractMayReturnNull();

  public native @Nullable Object nativeMayReturnNull();

  public Object unannotatedReturnNull() {
    return null;
  }

  public static class Sub {
    public Object subtypingInconsistency(@Nullable Object object) {
      return new Object();
    }
  }
}
