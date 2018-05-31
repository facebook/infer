/*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package com.facebook.infer.builtins;

public class InferBuiltins {

    public native static void __set_file_attribute(Object o);

    public native static void __set_mem_attribute(Object o);

    public native static void __set_locked_attribute(Object o);

    public native static void __delete_locked_attribute(Object o);

    public native static void _exit();

    private native static void __infer_assume(boolean condition);

    public static void assume(boolean condition) {
      __infer_assume(condition);
    }

    // use this instead of "assume o != null". being non-null and allocated are different to Infer
    public static void assume_allocated(Object o) {
      assume(o != null);
      o.hashCode();
    }

    public native static String __split_get_nth(String s, String sep, int n);

}
