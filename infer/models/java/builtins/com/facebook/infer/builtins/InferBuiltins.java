/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.builtins;

public class InferBuiltins {

  public static native void __set_file_attribute(Object o);

  public static native void __set_mem_attribute(Object o);

  public static native void __set_locked_attribute(Object o);

  public static native void __delete_locked_attribute(Object o);

  public static native void _exit();

  private static native void __infer_assume(boolean condition);

  public static void assume(boolean condition) {
    __infer_assume(condition);
  }

  // use this instead of "assume o != null". being non-null and allocated are different to Infer
  public static void assume_allocated(Object o) {
    assume(o != null);
    o.hashCode();
  }

  public static native String __split_get_nth(String s, String sep, int n);
}
