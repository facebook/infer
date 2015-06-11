/*
 * Copyright (c) 2009-2013 Monoidics ltd.
 * Copyright (c) 2013- Facebook.
 * All rights reserved.
 */

package com.facebook.infer.models;

public class InferBuiltins {

    public static void __set_file_attribute(Object o) {
    }

    public static void __set_mem_attribute(Object o) {
    }

    public static void __set_lock_attribute(Object o) {
    }

    public static void _exit() {
    }

    public static void assume(boolean condition) {
      while (!condition) {}
    }

    public static String __split_get_nth(String s, String sep, int n) {
        return null;
    }

}
