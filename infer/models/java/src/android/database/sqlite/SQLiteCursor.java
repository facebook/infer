/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package android.database.sqlite;

import android.database.Cursor;
import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;


public class SQLiteCursor extends Cursor {

    @Deprecated
    public SQLiteCursor(SQLiteDatabase db, SQLiteCursorDriver driver,
                        String editTable, SQLiteQuery query) {
        this(driver, editTable, query);
    }


    public SQLiteCursor(SQLiteCursorDriver driver, String editTable, SQLiteQuery query) {
        InferBuiltins.__set_file_attribute(this);
    }


    public int getCount() {
        return InferUndefined.int_undefined();
    }


    public String[] getColumnNames() {
        return new String[0];
    }

    public void close() {
        InferBuiltins.__set_mem_attribute(this);
    }

}
