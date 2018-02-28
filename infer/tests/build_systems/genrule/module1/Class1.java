/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
}
