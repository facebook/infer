/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import android.content.SharedPreferences;
import android.util.Log;

public class LoggingPrivateData {

  public void logSharedPreferencesDataBad(SharedPreferences prefs) {
    Log.d("tag", prefs.getString("some", "data"));
  }

  public void logSharedPreferencesDataInTagBad(SharedPreferences prefs) {
    Log.d(prefs.getString("some", "data"), "value");
  }

  public void logDataOk(SharedPreferences prefs) {
    Log.d("tag", "value");
  }

}
