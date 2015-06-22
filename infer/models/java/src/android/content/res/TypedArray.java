/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package android.content.res;

import com.facebook.infer.models.InferBuiltins;

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
