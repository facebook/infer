/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package junit.framework;

public class Assert {

    public static void assume(boolean condition) {
        if (!condition) {
            while (true) {
            }
        }
    }

    public static void assertTrue(boolean condition) {
        assume(condition);
    }

    public static void assertTrue(String message, boolean condition) {
        assume(condition);
    }

    public static void assertFalse(boolean condition) {
        assume(!condition);
    }

    public static void assertFalse(String message, boolean condition) {
        assume(!condition);
    }

}
