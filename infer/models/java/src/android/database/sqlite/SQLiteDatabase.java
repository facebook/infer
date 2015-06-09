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
import android.database.DatabaseErrorHandler;
import android.os.CancellationSignal;

import com.facebook.infer.models.InferUndefined;

import dalvik.system.CloseGuard;

import java.util.WeakHashMap;


public final class SQLiteDatabase {

    private SQLiteDatabase(String path, int openFlags, CursorFactory cursorFactory,
                           DatabaseErrorHandler errorHandler) {
    }


    public Cursor query(boolean distinct, String table, String[] columns,
                        String selection, String[] selectionArgs, String groupBy,
                        String having, String orderBy, String limit) {
            return queryWithFactory(null, distinct, table, columns, selection, selectionArgs,
                groupBy, having, orderBy, limit, null);
    }


    public Cursor query(boolean distinct, String table, String[] columns,
                        String selection, String[] selectionArgs, String groupBy,
                        String having, String orderBy, String limit, CancellationSignal cancellationSignal) {
            return queryWithFactory(null, distinct, table, columns, selection, selectionArgs,
                groupBy, having, orderBy, limit, cancellationSignal);
    }


    public Cursor queryWithFactory(CursorFactory cursorFactory,
                                   boolean distinct, String table, String[] columns,
                                   String selection, String[] selectionArgs, String groupBy,
                                   String having, String orderBy, String limit) {
        return queryWithFactory(cursorFactory, distinct, table, columns, selection,
                selectionArgs, groupBy, having, orderBy, limit, null);
    }


    public Cursor queryWithFactory(CursorFactory cursorFactory,
                                   boolean distinct, String table, String[] columns,
                                   String selection, String[] selectionArgs, String groupBy,
                                   String having, String orderBy, String limit, CancellationSignal cancellationSignal) {
        return rawQueryWithFactory(cursorFactory, null, selectionArgs, table, cancellationSignal);
    }

    public Cursor query(String table, String[] columns, String selection,
                        String[] selectionArgs, String groupBy, String having,
                        String orderBy) {

        return query(false, table, columns, selection, selectionArgs, groupBy,
                having, orderBy, null /* limit */);
    }

    public Cursor query(String table, String[] columns, String selection,
                        String[] selectionArgs, String groupBy, String having,
                        String orderBy, String limit) {

        return query(false, table, columns, selection, selectionArgs, groupBy,
                having, orderBy, limit);
    }

    public Cursor rawQuery(String sql, String[] selectionArgs) {
        return rawQueryWithFactory(null, sql, selectionArgs, null, null);
    }

    public Cursor rawQuery(String sql, String[] selectionArgs,
                           CancellationSignal cancellationSignal) {
        return rawQueryWithFactory(null, sql, selectionArgs, null, cancellationSignal);
    }

    public Cursor rawQueryWithFactory(
            CursorFactory cursorFactory, String sql, String[] selectionArgs,
            String editTable) {
        return rawQueryWithFactory(cursorFactory, sql, selectionArgs, editTable, null);
    }

    public Cursor rawQueryWithFactory(
            CursorFactory cursorFactory, String sql, String[] selectionArgs,
            String editTable, CancellationSignal cancellationSignal) {
        return new SQLiteCursor(null, editTable, null);
    }

    public interface CursorFactory {
        public Cursor newCursor(SQLiteDatabase db,
                                SQLiteCursorDriver masterQuery, String editTable,
                                SQLiteQuery query);
    }
}

