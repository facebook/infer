/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.lang.reflect;

import com.facebook.infer.builtins.InferUndefined;

public final class Array {

    public static Object newInstance(Class<?> componentType, int length)
            throws NegativeArraySizeException {
        String name = componentType.getName();
        if (length < 0) {
            throw new NegativeArraySizeException();
        }
        if (name == "int") {
            return new int[length];
        } else if (name == "short") {
            return new short[length];
        } else if (name == "byte") {
            return new byte[length];
        } else if (name == "boolean") {
            return new boolean[length];
        } else if (name == "long") {
            return new long[length];
        } else if (name == "float") {
            return new float[length];
        } else if (name == "double") {
            return new double[length];
        } else if (name == "char") {
            return new char[length];
        } else
            return InferUndefined.object_undefined();
    }

}
