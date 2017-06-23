/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package java.util;

import com.facebook.infer.builtins.InferUndefined;

public abstract class Vector<E> {

    protected E[] elementData;

    E elementData(int index) {
        return (E) elementData[index];
    }

    public Enumeration<E> elements() {
        return new Enumeration<E>() {
            int count;

            public boolean hasMoreElements() {
                return count > 0;
            }

            public E nextElement() {
                if (count > 0)
                    return (E) InferUndefined.object_undefined();
                else
                    throw new NoSuchElementException();
            }
        };
    }

    public E get(int index) {
        return elementData(index);
    }

    public int size() {
        return InferUndefined.nonneg_int();
    }

}
