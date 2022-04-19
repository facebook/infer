/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.res.AssetFileDescriptor;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.os.CancellationSignal;
import android.os.ParcelFileDescriptor;
import java.io.File;

public abstract class ContentProviders extends ContentProvider {

  File mFile;

  /* NOTE: all methods below should produce a taint warning. Their names do not follow our standard
   * naming convention because they are overrides */

  @Override
  public int bulkInsert(Uri uri, ContentValues[] values) {
    mFile = new File(uri.toString());
    return 0;
  }

  @Override
  public Bundle call(String method, String args, Bundle extras) {
    mFile = new File(method);
    return extras;
  }

  @Override
  public int delete(Uri uri, String selection, String[] selectionArgs) {
    mFile = new File(uri.toString());
    return 0;
  }

  @Override
  public Uri insert(Uri uri, ContentValues values) {
    mFile = new File(uri.toString());
    return null;
  }

  @Override
  public String getType(Uri uri) {
    mFile = new File(uri.toString());
    return null;
  }

  @Override
  public AssetFileDescriptor openAssetFile(Uri uri, String mode, CancellationSignal signal) {
    mFile = new File(uri.toString());
    return null;
  }

  @Override
  public ParcelFileDescriptor openFile(Uri uri, String mode, CancellationSignal signal) {
    mFile = new File(uri.toString());
    return null;
  }

  @Override
  public AssetFileDescriptor openTypedAssetFile(
      Uri uri, String mimeTypeFilter, Bundle opts, CancellationSignal signal) {
    mFile = new File(uri.toString());
    return null;
  }

  @Override
  public Cursor query(
      Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
    mFile = new File(uri.toString());
    return null;
  }

  @Override
  public int update(Uri uri, ContentValues values, String selection, String[] selectionArgs) {
    mFile = new File(uri.toString());
    return 0;
  }
}
