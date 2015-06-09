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

package android.content.res;

import android.graphics.drawable.Drawable;
import android.os.IBinder;
import android.util.AttributeSet;
import android.util.DisplayMetrics;
import android.util.LongSparseArray;
import android.util.TypedValue;
import com.facebook.infer.models.InferUndefined;
import libcore.icu.NativePluralRules;

import java.lang.ref.WeakReference;

public class Resources {
    static String TAG;
    private static boolean DEBUG_LOAD;
    private static boolean DEBUG_CONFIG;
    private static boolean DEBUG_ATTRIBUTES_CACHE;
    private static boolean TRACE_FOR_PRELOAD;
    private static boolean TRACE_FOR_MISS_PRELOAD;

    private static int ID_OTHER;

    private static Object sSync;

    private static LongSparseArray<Drawable.ConstantState>[] sPreloadedDrawables;
    private static LongSparseArray<Drawable.ConstantState> sPreloadedColorDrawables;
    private static LongSparseArray<ColorStateList> sPreloadedColorStateLists;

    private static boolean sPreloaded;
    private static int sPreloadedDensity;

    Object mAccessLock;
    Configuration mTmpConfig;
    TypedValue mTmpValue;
    LongSparseArray<WeakReference<Drawable.ConstantState>> mDrawableCache;
    LongSparseArray<WeakReference<ColorStateList>> mColorStateListCache;
    LongSparseArray<WeakReference<Drawable.ConstantState>> mColorDrawableCache;
    boolean mPreloading;

    TypedArray mCachedStyledAttributes;
    RuntimeException mLastRetrievedAttrs;

    private int mLastCachedXmlBlockIndex;
    private int[] mCachedXmlBlockIds;
    private XmlBlock[] mCachedXmlBlocks;

    AssetManager mAssets;
    private Configuration mConfiguration;
    DisplayMetrics mMetrics;
    private NativePluralRules mPluralRule;

    private CompatibilityInfo mCompatibilityInfo;
    private WeakReference<IBinder> mToken;

    public final class Theme {
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

    static private int LAYOUT_DIR_CONFIG;

}