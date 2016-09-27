/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.content.res;

import com.facebook.infer.builtins.InferBuiltins;

public class TypedArray {

    private Resources mResources;
    int[] mData;
    int[] mIndices;
    int mLength;

    public void recycle() {
        // Release resource
        if (mLength > 0) {
            InferBuiltins.__set_mem_attribute(this);
        }
    }

    public TypedArray(Resources resources, int[] data, int[] indices, int len) {
        mResources = resources;
        mData = data;
        mIndices = indices;
        mLength = len;

        // Acquire resource
        if (mLength > 0) {
            InferBuiltins.__set_file_attribute(this);
        }
    }

}
