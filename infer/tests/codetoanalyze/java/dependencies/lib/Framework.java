/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package lib;

public class Framework {

    public static Object returnNull() {
        return null;
    }

    private static String source() {
        return "AttackerControlled";
    }

    public static String getString() {
        return source();
    }

    public static String readFile(String s) {
        try (MyStream inputStream = new MyStream(s)) {
            return inputStream.readContent();
        }
    }

    public static String shouldPropagateTaint(String s) {
        return s;
    }

    enum Value {
        Left, Right
    }

    public static String doesNotPropagateTaint(String s) {
        switch (Value.valueOf(s)) {
            case Left:
                return "Left";
            case Right:
                return "Right";
            default:
                return "Unknown";
        }
    }
}
