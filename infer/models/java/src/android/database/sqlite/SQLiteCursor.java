/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.database.sqlite;

import android.database.Cursor;
import com.facebook.infer.builtins.InferBuiltins;
import com.facebook.infer.builtins.InferUndefined;


public class SQLiteCursor implements Cursor {

    @Deprecated
    public SQLiteCursor(SQLiteDatabase db, SQLiteCursorDriver driver,
                        String editTable, SQLiteQuery query) {
        this(driver, editTable, query);
    }


    public SQLiteCursor(SQLiteCursorDriver driver, String editTable, SQLiteQuery query) {
        InferBuiltins.__set_file_attribute(this);
    }

    public void close() {
        InferBuiltins.__set_mem_attribute(this);
    }

}
