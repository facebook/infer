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
import android.os.CancellationSignal;

import java.util.Map;
import java.util.regex.Pattern;

public class SQLiteQueryBuilder {
    private static String TAG;
    private static Pattern sLimitPattern;

    private Map<String, String> mProjectionMap;
    private String mTables;
    private StringBuilder mWhereClause;
    private boolean mDistinct;
    private SQLiteDatabase.CursorFactory mFactory;
    private boolean mStrict;

    public SQLiteQueryBuilder() {
        mDistinct = false;
        mFactory = null;
    }

    public Cursor query(SQLiteDatabase db, String[] projectionIn,
                        String selection, String[] selectionArgs, String groupBy,
                        String having, String sortOrder) {
        return query(db, projectionIn, selection, selectionArgs, groupBy, having, sortOrder,
                null /* limit */, null /* cancellationSignal */);
    }

    public Cursor query(SQLiteDatabase db, String[] projectionIn,
                        String selection, String[] selectionArgs, String groupBy,
                        String having, String sortOrder, String limit) {
        return query(db, projectionIn, selection, selectionArgs,
                groupBy, having, sortOrder, limit, null);
    }

    public Cursor query(SQLiteDatabase db, String[] projectionIn,
                        String selection, String[] selectionArgs, String groupBy,
                        String having, String sortOrder, String limit,
                        CancellationSignal cancellationSignal) {
        return new SQLiteCursor(null, null, null);
    }


}
