/*
 * Copyright (C) 2009 The Android Open Source Project
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
import android.os.Handler;
import android.os.RemoteException;
import dalvik.system.CloseGuard;


public class ContentProviderClient {
    private static String TAG;

    private static Handler sAnrHandler;

    private ContentResolver mContentResolver;
    private IContentProvider mContentProvider;
    private String mPackageName;
    private boolean mStable;

    private CloseGuard mGuard;

    private long mAnrTimeout;
    private NotRespondingRunnable mAnrRunnable;

    private boolean mReleased;


    ContentProviderClient(
            ContentResolver contentResolver, IContentProvider contentProvider, boolean stable) {
        mContentResolver = contentResolver;
        mContentProvider = contentProvider;
        mPackageName = contentResolver.mPackageName;
        mStable = stable;
    }

    public Cursor query(Uri url, String[] projection, String selection,
                        String[] selectionArgs, String sortOrder) throws RemoteException {
        return query(url, projection, selection, selectionArgs, sortOrder, null);
    }

    public Cursor query(Uri url, String[] projection, String selection, String[] selectionArgs,
                        String sortOrder, CancellationSignal cancellationSignal) throws RemoteException {
        return new SQLiteCursor(null, null, null);
    }

    private class NotRespondingRunnable {
    }


}
