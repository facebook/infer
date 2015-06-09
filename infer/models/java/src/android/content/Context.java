/*
 * Copyright (C) 2006 The Android Open Source Project
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

package android.content;

import android.content.res.TypedArray;
import android.util.AttributeSet;
import com.facebook.infer.models.InferUndefined;

public class Context {

    public static int MODE_PRIVATE;

    public static int MODE_WORLD_READABLE;

    public static int MODE_WORLD_WRITEABLE;

    public static int MODE_APPEND;

    public static int MODE_MULTI_PROCESS;

    public static int MODE_ENABLE_WRITE_AHEAD_LOGGING;

    public static int BIND_AUTO_CREATE;

    public static int BIND_DEBUG_UNBIND;

    public static int BIND_NOT_FOREGROUND;

    public static int BIND_ABOVE_CLIENT;

    public static int BIND_ALLOW_OOM_MANAGEMENT;

    public static int BIND_WAIVE_PRIORITY;

    public static int BIND_IMPORTANT;

    public static int BIND_ADJUST_WITH_ACTIVITY;

    public static int BIND_VISIBLE;

    public static int BIND_SHOWING_UI;

    public static int BIND_NOT_VISIBLE;

    public ContentResolver getContentResolver() {
        return new ContentResolver(this);
    }

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
