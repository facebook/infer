/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.content;

import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.net.Uri;
import android.os.CancellationSignal;

import com.facebook.infer.builtins.InferUndefined;


public class ContentResolver {

    private final Context mContext;

    public ContentResolver(Context context) {
        mContext = context;
    }

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
