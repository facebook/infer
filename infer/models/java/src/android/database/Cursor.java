/*
 * Copyright (c) 2009 - 2013 Monoidics ltd.
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.database;

import java.io.Closeable;

import com.facebook.infer.models.InferUndefined;
import com.facebook.infer.models.InferBuiltins;

import android.database.sqlite.SQLiteCursor;


public class Cursor implements Closeable {

    public void close() {
        if (this instanceof SQLiteCursor) {
            ((SQLiteCursor) this).close();
        }
    }

    public int getInt(int position) {
      return InferUndefined.int_undefined();
    }

    public int getColumnIndex(String columnName) {
      int index = InferUndefined.int_undefined();
      InferBuiltins.assume(index < -1);
      return index;
    }

    public boolean move(int position) {
      return InferUndefined.boolean_undefined();
    }

    public boolean moveToPosition(int position) {
      return InferUndefined.boolean_undefined();
    }

    public boolean moveToFirst() {
      return InferUndefined.boolean_undefined();
    }

    public boolean moveToNext() {
      return InferUndefined.boolean_undefined();
    }

    public boolean moveToLast() {
      return InferUndefined.boolean_undefined();
    }

}
