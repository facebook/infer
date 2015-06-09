/*
 * Copyright (C) 2007 The Android Open Source Project
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

package android.provider;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;

/**
 * The Media provider contains meta data for all available media on both internal
 * and external storage devices.
 */
public final class MediaStore {
    private static String TAG;

    public static String AUTHORITY;

    private static String CONTENT_AUTHORITY_SLASH;

    public static String ACTION_MTP_SESSION_END;

    public static String UNHIDE_CALL;

    public static String PARAM_DELETE_DATA;

    public static String INTENT_ACTION_MUSIC_PLAYER;

    public static String INTENT_ACTION_MEDIA_SEARCH;

    public static String INTENT_ACTION_MEDIA_PLAY_FROM_SEARCH;

    public static String INTENT_ACTION_TEXT_OPEN_FROM_SEARCH;

    public static String INTENT_ACTION_VIDEO_PLAY_FROM_SEARCH;

    public static String EXTRA_MEDIA_ARTIST;

    public static String EXTRA_MEDIA_ALBUM;

    public static String EXTRA_MEDIA_TITLE;

    public static String EXTRA_MEDIA_FOCUS;

    public static String EXTRA_SCREEN_ORIENTATION;

    public static String EXTRA_FULL_SCREEN;

    public static String EXTRA_SHOW_ACTION_ICONS;

    public static String EXTRA_FINISH_ON_COMPLETION;

    public static String INTENT_ACTION_STILL_IMAGE_CAMERA;

    public static String INTENT_ACTION_STILL_IMAGE_CAMERA_SECURE;

    public static String INTENT_ACTION_VIDEO_CAMERA;

    public static String ACTION_IMAGE_CAPTURE;

    public static String ACTION_IMAGE_CAPTURE_SECURE;

    public static String ACTION_VIDEO_CAPTURE;

    public static String EXTRA_VIDEO_QUALITY;

    public static String EXTRA_SIZE_LIMIT;

    public static String EXTRA_DURATION_LIMIT;

    public static String EXTRA_OUTPUT;

    public static String UNKNOWN_STRING;


    public static final class Images {


        public static final class Media {
            public static final Cursor query(ContentResolver cr, Uri uri, String[] projection) {
                return cr.query(uri, projection, null, null, null);
            }

            public static final Cursor query(ContentResolver cr, Uri uri, String[] projection,
                                             String where, String orderBy) {
                return cr.query(uri, projection, where, null, orderBy);
            }

            public static final Cursor query(ContentResolver cr, Uri uri, String[] projection,
                                             String selection, String[] selectionArgs, String orderBy) {
                return cr.query(uri, projection, selection, selectionArgs, orderBy);
            }
        }
    }
}
