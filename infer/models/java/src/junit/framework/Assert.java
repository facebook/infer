/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package junit.framework;

import com.facebook.infer.builtins.InferBuiltins;

public class Assert {

  public static void assertTrue(boolean condition) {
    InferBuiltins.assume(condition);
  }

  public static void assertTrue(String message, boolean condition) {
    InferBuiltins.assume(condition);
  }

  public static void assertFalse(boolean condition) {
    InferBuiltins.assume(!condition);
  }

  public static void assertFalse(String message, boolean condition) {
    InferBuiltins.assume(!condition);
  }
}
