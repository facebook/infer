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
import android.database.CursorWrapper;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteQueryBuilder;
import android.os.RemoteException;
import android.provider.MediaStore;

public class CursorLeaks {

  public int cursorClosed(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    try {
      return cursor.getCount();
    } finally {
      cursor.close();
    }
  }

  public Object cursorClosedCheckNull(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    Object value = null;

    try {
      if (cursor == null) {
        return null;
      }

      value = cursor.getString(0);
    } finally {
      if (cursor != null) {
        cursor.close();
      }
    }
    return value;
  }

  public Object cursorClosedCheckNullCheckClosed_FP(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    Object value = null;

    try {
      if (cursor == null) {
        return null;
      }

      value = cursor.getString(0);
    } finally {
      if (cursor != null && !cursor.isClosed()) {
        cursor.close();
      }
    }
    return value;
  }

  public int cursorNotClosed(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    return cursor.getCount();
  }

  Context mContext;
  ContentResolver mContentResolver;

  public int getImageCountHelperNotClosed(String customClause) {
    String[] projection = {"COUNT(*)"};

    String selectionClause = selectionClause = customClause;

    Cursor cursor =
        mContext.getContentResolver().query(null, projection, selectionClause, null, null);

    if (cursor != null) {
      int count = cursor.getInt(0);
      // cursor.close();
      return count;
    } else {
      return 0;
    }
  }

  public int getImageCountHelperClosed(String customClause) {
    String[] projection = {"COUNT(*)"};

    String selectionClause = selectionClause = customClause;

    Cursor cursor =
        mContext.getContentResolver().query(null, projection, selectionClause, null, null);

    if (cursor != null) {
      int count = cursor.getInt(0);
      cursor.close();
      return count;
    } else {
      return 0;
    }
  }

  public int getBucketCountNotClosed() {
    Cursor cursor = MediaStore.Images.Media.query(mContentResolver, null, null, null, null, null);
    if (cursor == null) {
      return 0;
    } else {
      int count = 0;
      while (cursor.moveToNext()) {
        count++;
      }
      return count;
    }
  }

  public int getBucketCountClosed() {
    Cursor cursor = MediaStore.Images.Media.query(mContentResolver, null, null, null, null, null);
    if (cursor == null) {
      return 0;
    } else {
      try {
        int count = 0;
        while (cursor.moveToNext()) {
          count++;
        }
        return count;
      } finally {
        cursor.close();
      }
    }
  }

  private void queryUVMLegacyDbNotClosed() {
    SQLiteQueryBuilder builder = new SQLiteQueryBuilder();
    builder.setTables("");
    Cursor cursor = builder.query(null, null, "", null, null, null, null);
    if (cursor != null) cursor.moveToFirst();
  }

  private void queryUVMLegacyDbClosed() {
    SQLiteQueryBuilder builder = new SQLiteQueryBuilder();
    builder.setTables("");
    Cursor cursor = builder.query(null, null, "", null, null, null, null);
    if (cursor != null) cursor.close();
  }

  public int completeDownloadClosed(DownloadManager downloadManager) {
    DownloadManager.Query query = new DownloadManager.Query();
    Cursor cursor = (Cursor) null;
    try {
      cursor = downloadManager.query(query);
      if (cursor == null) {
        return 0;
      } else {
        return cursor.getColumnIndex(DownloadManager.COLUMN_STATUS);
      }
    } finally {
      if (cursor != null) cursor.close();
    }
  }

  public int completeDownloadNotClosed(DownloadManager downloadManager) {
    DownloadManager.Query query = new DownloadManager.Query();
    Cursor cursor = null;
    try {
      cursor = downloadManager.query(query);
      if (cursor == null) {
        return 0;
      } else {
        return cursor.getColumnIndex(DownloadManager.COLUMN_STATUS);
      }
    } finally {
      // cursor.close();
    }
  }

  private void loadPrefsFromContentProviderClosed() {
    ContentProviderClient contentProviderClient = mContentResolver.acquireContentProviderClient("");
    if (contentProviderClient != null) {
      Cursor cursor = null;
      try {
        try {
          cursor = contentProviderClient.query(null, null, null, null, null);
        } catch (RemoteException ex) {
        }
      } finally {
        if (cursor != null) {
          cursor.close();
        }
      }
    }
  }

  private void loadPrefsFromContentProviderNotClosed() {
    ContentProviderClient contentProviderClient = mContentResolver.acquireContentProviderClient("");
    if (contentProviderClient == null) return;
    Cursor cursor = null;
    try {
      try {
        cursor = contentProviderClient.query(null, null, null, null, null);
      } catch (RemoteException ex) {
      }
    } finally {
      if (cursor != null) {
        // cursor.close();
      }
    }
  }

  class NamedCursor extends CursorWrapper {
    private String mName;

    NamedCursor(Cursor cursor, String name) {
      super(cursor);
      mName = name;
    }
  }

  public Cursor cursorWrapperReturned(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    return new NamedCursor(cursor, "abc");
  }

  // TODO (#7474990): investigate why is Infer reporting a resource leak here
  //  public void cursorWrapperClosed(SQLiteDatabase sqLiteDatabase) {
  //    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
  //    Cursor c = new NamedCursor(cursor, "abc");
  //    c.close();
  //  }

  native NamedCursor createWrapper(Cursor cursor);

  public NamedCursor cursorAttachedTheWrapper(SQLiteDatabase sqLiteDatabase) {
    Cursor cursor = sqLiteDatabase.query("events", null, null, null, null, null, null);
    return createWrapper(cursor);
  }
}
