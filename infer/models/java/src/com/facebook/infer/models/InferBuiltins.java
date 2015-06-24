/*
 * Copyright (c) 2009-2013 Monoidics ltd.
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package com.facebook.infer.models;

public class InferBuiltins {

    public native static void __set_file_attribute(Object o);

    public native static void __set_mem_attribute(Object o);

    public native static void __set_lock_attribute(Object o);

    public native static void _exit();

    private native static void __infer_assume(boolean condition);

    public static void assume(boolean condition) {
      __infer_assume(condition);
    }

    public native static String __split_get_nth(String s, String sep, int n);

}
