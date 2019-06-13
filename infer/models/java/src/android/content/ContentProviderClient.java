/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package android.content;

import android.database.Cursor;
import android.database.sqlite.SQLiteCursor;
import android.net.Uri;
import android.os.CancellationSignal;
import android.os.RemoteException;
import com.facebook.infer.builtins.InferUndefined;

public class ContentProviderClient {

  private ContentResolver mContentResolver;
  private IContentProvider mContentProvider;
  private String mPackageName;
  private boolean mStable;

  ContentProviderClient(
      ContentResolver contentResolver, IContentProvider contentProvider, boolean stable) {
    mContentResolver = contentResolver;
    mContentProvider = contentProvider;
    mPackageName = (String) InferUndefined.object_undefined();
    mStable = stable;
  }

  public Cursor query(
      Uri url, String[] projection, String selection, String[] selectionArgs, String sortOrder)
      throws RemoteException {
    return query(url, projection, selection, selectionArgs, sortOrder, null);
  }

  public Cursor query(
      Uri url,
      String[] projection,
      String selection,
      String[] selectionArgs,
      String sortOrder,
      CancellationSignal cancellationSignal)
      throws RemoteException {
    return new SQLiteCursor(null, null, null);
  }

  private class NotRespondingRunnable {}
}
