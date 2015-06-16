/*
 * Copyright (C) 2008 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package android.content.res;

import android.util.TypedValue;
import com.facebook.infer.models.InferBuiltins;

public class TypedArray {

    private Resources mResources;
    int[] mData;
    int[] mIndices;
    int mLength;
    TypedValue mValue = new TypedValue();

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
