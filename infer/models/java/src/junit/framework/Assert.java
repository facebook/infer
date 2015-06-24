/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package junit.framework;

import com.facebook.infer.models.InferBuiltins;

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
