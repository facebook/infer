/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package android.database.sqlite;

import android.database.Cursor;
import android.os.CancellationSignal;

public class SQLiteQueryBuilder {

  public Cursor query(
      SQLiteDatabase db,
      String[] projectionIn,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String sortOrder) {
    return query(
        db,
        projectionIn,
        selection,
        selectionArgs,
        groupBy,
        having,
        sortOrder,
        null /* limit */,
        null /* cancellationSignal */);
  }

  public Cursor query(
      SQLiteDatabase db,
      String[] projectionIn,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String sortOrder,
      String limit) {
    return query(
        db, projectionIn, selection, selectionArgs, groupBy, having, sortOrder, limit, null);
  }

  public Cursor query(
      SQLiteDatabase db,
      String[] projectionIn,
      String selection,
      String[] selectionArgs,
      String groupBy,
      String having,
      String sortOrder,
      String limit,
      CancellationSignal cancellationSignal) {
    return new SQLiteCursor(null, null, null);
  }
}
