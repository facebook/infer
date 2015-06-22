/*
* Copyright (c) 2013- Facebook.
* All rights reserved.
*/

package android.app;

import android.content.ContentResolver;
import android.database.Cursor;

public class DownloadManager {

    private ContentResolver mResolver;
    private String mPackageName;

    public DownloadManager(ContentResolver resolver, String packageName) {
        mResolver = resolver;
        mPackageName = packageName;
    }

    public static class Query {
    }

    public Cursor query(Query query) {
        return mResolver.query(null, null, null, null, null);
    }


}
