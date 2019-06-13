/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package android.database;

import java.io.IOException;

public class CursorWrapper implements Cursor {
  protected final Cursor mCursor;

  public CursorWrapper(Cursor cursor) {
    mCursor = cursor;
  }

  public void close() {
    try {
      mCursor.close();
    } catch (IOException e) {
    }
  }
}
