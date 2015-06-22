/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package android.database;


public class CursorWrapper extends Cursor {
  protected final Cursor mCursor;

  public CursorWrapper(Cursor cursor) {
    mCursor = cursor;
  }

  public void close() {
    mCursor.close();
  }
}
