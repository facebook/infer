/*
 * Copyright (C) 2006 The Android Open Source Project
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

package android.database.sqlite;

import android.database.Cursor;
import com.facebook.infer.models.InferBuiltins;
import com.facebook.infer.models.InferUndefined;


public class SQLiteCursor extends Cursor {

    private String mEditTable;

    @Deprecated
    public SQLiteCursor(SQLiteDatabase db, SQLiteCursorDriver driver,
                        String editTable, SQLiteQuery query) {
        this(driver, editTable, query);
    }


    public SQLiteCursor(SQLiteCursorDriver driver, String editTable, SQLiteQuery query) {
        mEditTable = new String();
        InferBuiltins.__set_file_attribute(mEditTable);
    }


    public int getCount() {
        return InferUndefined.int_undefined();
    }


    public String[] getColumnNames() {
        return new String[0];
    }

    public void close() {
        InferBuiltins.__set_mem_attribute(mEditTable);
    }
}
