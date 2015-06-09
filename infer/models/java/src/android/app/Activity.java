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

package android.app;

import android.content.ComponentName;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import android.content.res.TypedArray;
import android.database.Cursor;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.text.SpannableStringBuilder;
import android.util.ArrayMap;
import android.util.AttributeSet;
import android.util.SparseArray;
import android.view.*;
import com.android.internal.app.ActionBarImpl;
import com.facebook.infer.models.InferUndefined;

import java.util.ArrayList;
import java.util.HashMap;

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
