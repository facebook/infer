/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import java.io.IOException;
import java.net.URISyntaxException;

import android.app.Activity;
import android.content.Intent;
import android.net.Uri;

import org.xmlpull.v1.XmlPullParserException;

public class Intents {

  Activity activity;

  public void callAllSinks(Intent i) {
    activity.bindService(i, null, 0);
    activity.sendBroadcast(i);
    activity.sendBroadcastAsUser(i, null);
    activity.sendOrderedBroadcast(i, null);
    activity.sendStickyBroadcast(i);
    activity.sendStickyBroadcastAsUser(i, null);
    activity.sendStickyOrderedBroadcast(i, null, null, 0, null, null);
    activity.sendStickyOrderedBroadcastAsUser(i, null, null, null, 0, null, null);
    activity.startActivities(new Intent[] { i });
    activity.startActivity(i);
    activity.startActivityForResult(i, 0);
    activity.startActivityIfNeeded(i, 0);
    activity.startActivityFromChild(null, i, 0);
    activity.startActivityFromFragment(null, i, 0);
    activity.startService(i);
  }

  private native int rand();

  public Intent returnAllSources() throws
    IOException, URISyntaxException, XmlPullParserException {
    switch (rand()) {
    case 1:
      return Intent.parseUri(null, 0);
    case 2:
      return Intent.parseIntent(null, null, null);
    }
    return null;
  }

  public void callAllSinksBad(String uri) throws
    IOException, URISyntaxException, XmlPullParserException {
    Intent intent = returnAllSources();
    callAllSinks(intent);
  }

}
