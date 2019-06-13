/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.google.common.base;

import static com.facebook.infer.builtins.InferBuiltins.assume;

import javax.annotation.Nullable;

public final class Preconditions {

  public static <T> T checkNotNull(T reference) {
    assume(reference != null);
    return reference;
  }

  public static <T> T checkNotNull(T reference, @Nullable Object errorMessage) {
    return checkNotNull(reference);
  }

  public static <T> T checkNotNull(
      T reference, @Nullable String errorMessageTemplate, @Nullable Object... errorMessageArgs) {
    return checkNotNull(reference);
  }

  public static void checkState(boolean expression) {
    assume(expression);
  }

  public static void checkState(boolean expression, @Nullable Object errorMessage) {
    assume(expression);
  }

  public static void checkState(
      boolean expression,
      @Nullable String errorMessageTemplate,
      @Nullable Object... errorMessageArgs) {
    assume(expression);
  }

  public static void checkArgument(boolean expression) {
    assume(expression);
  }

  public static void checkArgument(boolean expression, @Nullable Object errorMessage) {
    assume(expression);
  }

  public static void checkArgument(
      boolean expression,
      @Nullable String errorMessageTemplate,
      @Nullable Object... errorMessageArgs) {
    assume(expression);
  }
}
