/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package android.content;

import com.facebook.infer.builtins.InferBuiltins;

public class ContentValues {

  /**
   * We want to treat this as a sink for both privacy and security purposes.
   *
   * Privacy: The purpose of ContentValues is to feed information into a ContentProvider, a core
   * Android component that can expose data for other Android applications to query. Thus, you do
   * not want secret information to flow into ContentValues because you don't want to give other
   * apps an interface that lets them query your secret information. There's a possibility for false
   * positives here because ContentProviders can control access to information via permissions, but
   * in general it's just a bad idea to let secret info into a ContentProvider.
   *
   * Security: You don't want untrusted external content to flow into ContentValues because it may
   * be used to store data in a ContentProvider, potentially giving an external app control over
   * what goes into the database backing the ContentProvider/giving them direct access to make
   * queries on your content provider (rather than exposing a small set of trusted queries, which is
   * what should be done). This could have any number of security implications depending on how the
   * ContentProvider is used.
   **/
  public void put(String key, String value) {
    InferBuiltins.__check_untainted(key);
    InferBuiltins.__check_untainted(value);
  }

}
