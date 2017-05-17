/*
 * Copyright (c) 2004 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package com.facebook.infer.annotation;

import com.facebook.infer.builtins.InferBuiltins;

import javax.annotation.Nullable;

public class Assertions {

  public static <T> T assumeNotNull(@Nullable T object) {
    InferBuiltins.assume_allocated(object);
    return object;
  }

  public static <T> T assumeNotNull(@Nullable T object, String explanation) {
    InferBuiltins.assume_allocated(object);
    return object;
  }

  public static <T> T assertNotNull(@Nullable T object) {
    InferBuiltins.assume_allocated(object);
    return object;
  }

  public static <T> T assertNotNull(@Nullable T object, String explanation) {
    InferBuiltins.assume_allocated(object);
    return object;
  }

  public static void assumeCondition(boolean condition) {
    InferBuiltins.assume(condition);
  }

  public static void assumeCondition(boolean condition, String explanation) {
    InferBuiltins.assume(condition);
  }

  public static void assertCondition(boolean condition) {
    InferBuiltins.assume(condition);
  }

  public static void assertCondition(boolean condition, String explanation) {
    InferBuiltins.assume(condition);
  }
}
