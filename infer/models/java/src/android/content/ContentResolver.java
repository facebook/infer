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

import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.net.Uri;
import android.os.CancellationSignal;

import com.facebook.infer.models.InferUndefined;

import java.util.Random;


public class ContentResolver {

    public static String SYNC_EXTRAS_ACCOUNT;

    public static String SYNC_EXTRAS_EXPEDITED;

    public static String SYNC_EXTRAS_FORCE;

    public static String SYNC_EXTRAS_IGNORE_SETTINGS;

    public static String SYNC_EXTRAS_IGNORE_BACKOFF;

    public static String SYNC_EXTRAS_DO_NOT_RETRY;

    public static String SYNC_EXTRAS_MANUAL;

    public static String SYNC_EXTRAS_UPLOAD;

    public static String SYNC_EXTRAS_OVERRIDE_TOO_MANY_DELETIONS;

    public static String SYNC_EXTRAS_DISCARD_LOCAL_DELETIONS;

    public static String SYNC_EXTRAS_EXPECTED_UPLOAD;

    public static String SYNC_EXTRAS_EXPECTED_DOWNLOAD;

    public static String SYNC_EXTRAS_PRIORITY;

    public static String SYNC_EXTRAS_DISALLOW_METERED;

    public static String SYNC_EXTRAS_INITIALIZE;

    public static Intent ACTION_SYNC_CONN_STATUS_CHANGED;

    public static String SCHEME_CONTENT;
    public static String SCHEME_ANDROID_RESOURCE;
    public static String SCHEME_FILE;

    public static String CURSOR_ITEM_BASE_TYPE;

    public static String CURSOR_DIR_BASE_TYPE;

    public static int SYNC_ERROR_SYNC_ALREADY_IN_PROGRESS;

    public static int SYNC_ERROR_AUTHENTICATION;

    public static int SYNC_ERROR_IO;

    public static int SYNC_ERROR_PARSE;

    public static int SYNC_ERROR_CONFLICT;

    public static int SYNC_ERROR_TOO_MANY_DELETIONS;

    public static int SYNC_ERROR_TOO_MANY_RETRIES;

    public static int SYNC_ERROR_INTERNAL;

    private static String[] SYNC_ERROR_NAMES;

    public static int SYNC_OBSERVER_TYPE_SETTINGS;
    public static int SYNC_OBSERVER_TYPE_PENDING;
    public static int SYNC_OBSERVER_TYPE_ACTIVE;

    public static int SYNC_OBSERVER_TYPE_STATUS;

    public static int SYNC_OBSERVER_TYPE_ALL;

    private static boolean ENABLE_CONTENT_SAMPLE;
    private static int SLOW_THRESHOLD_MILLIS;
    private Random mRandom;

    public ContentResolver(Context context) {
        mContext = context;
    }

    public static String CONTENT_SERVICE_NAME;

    private static IContentService sContentService;
    private final Context mContext;
    String mPackageName;
    private static String TAG;

    public final Cursor query(Uri uri, String[] projection,
                              String selection, String[] selectionArgs, String sortOrder) {
        if (InferUndefined.boolean_undefined()) {
          return null;
        } else {
          return query(uri, projection, selection, selectionArgs, sortOrder, null);
        }
    }

    public final Cursor query(final Uri uri, String[] projection,
                              String selection, String[] selectionArgs, String sortOrder,
                              CancellationSignal cancellationSignal) {
        if (InferUndefined.boolean_undefined()) {
          return null;
        } else {
          return new SQLiteCursor(null, null, null);
        }
    }

    public final ContentProviderClient acquireContentProviderClient(Uri uri) {
          return new ContentProviderClient(this, null, true);
    }

    public final ContentProviderClient acquireContentProviderClient(String name) {
          return new ContentProviderClient(this, null, true);
    }
}
