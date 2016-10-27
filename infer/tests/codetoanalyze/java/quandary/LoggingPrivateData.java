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
import android.location.Location;
import android.telephony.TelephonyManager;
import android.util.Log;

public class LoggingPrivateData {

  public void logSharedPreferencesDataBad(SharedPreferences prefs) {
    Log.d("tag", prefs.getString("some", "data"));
  }

  public void logSharedPreferencesDataInTagBad(SharedPreferences prefs) {
    Log.d(prefs.getString("some", "data"), "value");
  }

  static class StringWrapper extends Throwable {
    private String mStr;

    @Override
    public String toString() {
      return mStr;
    }
  }

  public void logSharedPreferencesDataIndirectBad(SharedPreferences prefs) {
    StringWrapper wrapper = new StringWrapper();
    wrapper.mStr = prefs.getString("some", "data");
    Log.w("tag", wrapper);
  }

  public void logDataOk(SharedPreferences prefs) {
    Log.d("tag", "value");
  }

  private native int rand();

  public String returnAllSources(Location l, TelephonyManager t) {
    switch (rand()) {
    case 1:
      return String.valueOf(l.getAltitude());
    case 2:
      return String.valueOf(l.getBearing());
    case 3:
      return String.valueOf(l.getLatitude());
    case 4:
      return String.valueOf(l.getLongitude());
    case 5:
      return String.valueOf(l.getSpeed());
    case 6:
      return t.getDeviceId();
    case 7:
      return t.getLine1Number();
    case 8:
      return t.getSimSerialNumber();
    case 9:
      return t.getSubscriberId();
    case 10:
      return t.getVoiceMailNumber();
    }
    return null;
  }

  public void logAllSourcesBad(Location l, TelephonyManager t) {
    String source = returnAllSources(l, t);
    Log.d("tag", source);
  }

}
