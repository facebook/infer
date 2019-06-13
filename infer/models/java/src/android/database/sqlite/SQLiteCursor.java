/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package android.database.sqlite;

import android.database.Cursor;
import com.facebook.infer.builtins.InferBuiltins;

public class SQLiteCursor implements Cursor {

  @Deprecated
  public SQLiteCursor(
      SQLiteDatabase db, SQLiteCursorDriver driver, String editTable, SQLiteQuery query) {
    this(driver, editTable, query);
  }

  public SQLiteCursor(SQLiteCursorDriver driver, String editTable, SQLiteQuery query) {
    InferBuiltins.__set_file_attribute(this);
  }

  public void close() {
    InferBuiltins.__set_mem_attribute(this);
  }
}
