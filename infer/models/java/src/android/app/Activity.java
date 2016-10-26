/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.app;

import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.view.ContextThemeWrapper;

import com.facebook.infer.builtins.InferUndefined;

public abstract class Activity extends ContextThemeWrapper {

    public TypedArray obtainStyledAttributes(int[] attrs) {
        return new TypedArray(null, attrs, attrs, 1);
    }

    public TypedArray obtainStyledAttributes(int resid, int[] attrs)
            throws NotFoundException {
        if (InferUndefined.boolean_undefined()) {
            throw new NotFoundException();
        }
        return new TypedArray(null, attrs, attrs, 1);
    }

    public TypedArray obtainStyledAttributes(AttributeSet set,
                                             int[] attrs, int defStyleAttr, int defStyleRes) {
        return new TypedArray(null, attrs, attrs, 1);
    }

    public static class NotFoundException extends RuntimeException {
        public NotFoundException() {
        }

        public NotFoundException(String name) {
            super(name);
        }
    }

    public TypedArray obtainAttributes(AttributeSet set, int[] attrs) {
        return new TypedArray(null, attrs, attrs, 1);
    }

}
