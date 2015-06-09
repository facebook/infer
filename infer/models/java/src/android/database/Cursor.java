/*
* Copyright (c) 2009-2013 Monoidics ltd.
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package android.database;

import android.database.sqlite.SQLiteCursor;

public class Cursor {

    public void close() {
        if (this instanceof SQLiteCursor) {
            ((SQLiteCursor) this).close();
        }
    }

}
