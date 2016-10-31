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

  private native int rand();

  public void callAllSinksBad(Activity activity, String uri) throws
    IOException, URISyntaxException, XmlPullParserException {
    Intent intent = null;

    switch (rand()) {
    case 1:
      intent = Intent.parseUri(null, 0);
      break;
    case 2:
      intent = Intent.parseIntent(null, null, null);
      break;
    }

    activity.bindService(intent, null, 0);
    activity.sendBroadcast(intent);
    activity.sendBroadcastAsUser(intent, null);
    activity.sendOrderedBroadcast(intent, null);
    activity.sendStickyBroadcast(intent);
    activity.sendStickyBroadcastAsUser(intent, null);
    activity.sendStickyOrderedBroadcast(intent, null, null, 0, null, null);
    activity.sendStickyOrderedBroadcastAsUser(intent, null, null, null, 0, null, null);
    activity.startActivities(new Intent[] { intent });
    activity.startActivity(intent);
    activity.startActivityForResult(intent, 0);
    activity.startActivityIfNeeded(intent, 0);
    activity.startActivityFromChild(null, intent, 0);
    activity.startActivityFromFragment(null, intent, 0);
    activity.startService(intent); // 2 sinks * 15 sources = 30 expected reports
  }

}
