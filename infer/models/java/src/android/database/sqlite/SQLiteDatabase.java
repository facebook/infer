/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package android.database.sqlite;

import android.database.Cursor;
import android.database.DatabaseErrorHandler;
import android.os.CancellationSignal;

public final class SQLiteDatabase {

  private SQLiteDatabase(
      String path, int openFlags, CursorFactory cursorFactory, DatabaseErrorHandler errorHandler) {}

  public Cursor query(
      boolean distinct,
      String table,
      String[] columns,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String orderBy,
      String limit) {
    return queryWithFactory(
        null,
        distinct,
        table,
        columns,
        selection,
        selectionArgs,
        groupBy,
        having,
        orderBy,
        limit,
        null);
  }

  public Cursor query(
      boolean distinct,
      String table,
      String[] columns,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String orderBy,
      String limit,
      CancellationSignal cancellationSignal) {
    return queryWithFactory(
        null,
        distinct,
        table,
        columns,
        selection,
        selectionArgs,
        groupBy,
        having,
        orderBy,
        limit,
        cancellationSignal);
  }

  public Cursor queryWithFactory(
      CursorFactory cursorFactory,
      boolean distinct,
      String table,
      String[] columns,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String orderBy,
      String limit) {
    return queryWithFactory(
        cursorFactory,
        distinct,
        table,
        columns,
        selection,
        selectionArgs,
        groupBy,
        having,
        orderBy,
        limit,
        null);
  }

  public Cursor queryWithFactory(
      CursorFactory cursorFactory,
      boolean distinct,
      String table,
      String[] columns,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String orderBy,
      String limit,
      CancellationSignal cancellationSignal) {
    return rawQueryWithFactory(cursorFactory, null, selectionArgs, table, cancellationSignal);
  }

  public Cursor query(
      String table,
      String[] columns,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String orderBy) {

    return query(
        false,
        table,
        columns,
        selection,
        selectionArgs,
        groupBy,
        having,
        orderBy,
        null /* limit */);
  }

  public Cursor query(
      String table,
      String[] columns,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String orderBy,
      String limit) {

    return query(false, table, columns, selection, selectionArgs, groupBy, having, orderBy, limit);
  }

  public Cursor rawQuery(String sql, String[] selectionArgs) {
    return rawQueryWithFactory(null, sql, selectionArgs, null, null);
  }

  public Cursor rawQuery(
      String sql, String[] selectionArgs, CancellationSignal cancellationSignal) {
    return rawQueryWithFactory(null, sql, selectionArgs, null, cancellationSignal);
  }

  public Cursor rawQueryWithFactory(
      CursorFactory cursorFactory, String sql, String[] selectionArgs, String editTable) {
    return rawQueryWithFactory(cursorFactory, sql, selectionArgs, editTable, null);
  }

  public Cursor rawQueryWithFactory(
      CursorFactory cursorFactory,
      String sql,
      String[] selectionArgs,
      String editTable,
      CancellationSignal cancellationSignal) {
    return new SQLiteCursor(null, editTable, null);
  }

  public interface CursorFactory {
    public Cursor newCursor(
        SQLiteDatabase db, SQLiteCursorDriver masterQuery, String editTable, SQLiteQuery query);
  }
}
