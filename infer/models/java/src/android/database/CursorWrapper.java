/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.database;

import com.facebook.infer.builtins.InferUndefined;
import com.facebook.infer.builtins.InferBuiltins;

import java.io.IOException;

public class CursorWrapper implements Cursor {
  protected final Cursor mCursor;

  public CursorWrapper(Cursor cursor) {
    mCursor = cursor;
  }

  public void close() {
    try {
      mCursor.close();
    } catch (IOException e) {}
  }

}
