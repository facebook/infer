/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import android.app.DownloadManager;
import android.content.ContentProviderClient;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteQueryBuilder;
import android.os.RemoteException;
import android.provider.MediaStore;

public class CursorNPEs {

  public int cursorNPEfromQuery(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    try {
      return cursor.getCount();
    } finally {
      cursor.close();
    }
  }

  Context mContext;
  ContentResolver mContentResolver;

  public void FN_cursorFromContentResolverNPE(String customClause) {
    String[] projection = {"COUNT(*)"};

    String selectionClause = selectionClause = customClause;

    Cursor cursor =
        mContext.getContentResolver().query(null, projection, selectionClause, null, null);

    cursor.close();
  }

  public void FN_cursorFromMediaNPE() {
    Cursor cursor = MediaStore.Images.Media.query(mContentResolver, null, null, null, null, null);
    cursor.close();
  }

  private void cursorFromSQLiteQueryBuilderNPE() {
    SQLiteQueryBuilder builder = new SQLiteQueryBuilder();
    builder.setTables("");
    Cursor cursor = builder.query(null, null, "", null, null, null, null);
    cursor.close();
  }

  public int FN_cursorFromDownloadManagerNPE(DownloadManager downloadManager) {
    DownloadManager.Query query = new DownloadManager.Query();
    Cursor cursor = null;
    try {
      cursor = downloadManager.query(query);
      return cursor.getColumnIndex(DownloadManager.COLUMN_STATUS);
    } finally {
      if (cursor != null) cursor.close();
    }
  }

  private void cursorFromContentProviderClient() {
    ContentProviderClient contentProviderClient = mContentResolver.acquireContentProviderClient("");
    if (contentProviderClient != null) {
      Cursor cursor = null;
      try {
        try {
          cursor = contentProviderClient.query(null, null, null, null, null);
          cursor.moveToFirst();
        } catch (RemoteException ex) {
        }
      } finally {
        if (cursor != null) {
          cursor.close();
        }
      }
    }
  }
}
