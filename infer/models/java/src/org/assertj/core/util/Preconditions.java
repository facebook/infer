/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package org.assertj.core.util;

import static com.facebook.infer.builtins.InferBuiltins.assume;

public final class Preconditions {

  public static void checkArgument(
      boolean expression, String errorMessageTemplate, Object... errorMessageArgs) {
    assume(expression);
  }

  public static <T> T checkNotNull(T reference) {
    assume(reference != null);
    return reference;
  }

  public static <T> T checkNotNull(T reference, String message) {
    assume(reference != null);
    return reference;
  }

  public static void checkState(
      boolean expression, String errorMessageTemplate, Object... errorMessageArgs) {
    assume(expression);
  }
}
