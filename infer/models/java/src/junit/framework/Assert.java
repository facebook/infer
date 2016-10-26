/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
