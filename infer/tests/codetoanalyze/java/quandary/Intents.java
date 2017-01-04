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
import android.content.Context;
import android.content.Intent;
import android.content.res.Resources;
import android.net.Uri;
import android.os.Bundle;

import com.facebook.infer.builtins.InferTaint;

import org.xmlpull.v1.XmlPullParserException;

public class Intents {

  private native int rand();

  public void callAllActivitySinksBad(Activity activity, String uri) throws
    IOException, URISyntaxException, XmlPullParserException {
    Intent intent = (Intent) InferTaint.inferSecretSource();

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
    activity.startService(intent); // 15 sinks, 15 expected reports
  }

  public void callAllIntentSinksBad(Intent cleanIntent) throws
    IOException, URISyntaxException, XmlPullParserException {
    String taintedString = cleanIntent.getStringExtra("");
    Intent taintedIntent = (Intent) InferTaint.inferSecretSource();
    Resources taintedResources = (Resources) ((Object) taintedString);
    Uri taintedUri = taintedIntent.getData();

    Intent intent = new Intent();
    intent.fillIn(taintedIntent, 0);
    intent.makeMainSelectorActivity(taintedString, null);
    intent.parseIntent(taintedResources, null, null);
    intent.parseUri(taintedString, 0);
    intent.replaceExtras(taintedIntent);
    intent.setAction(taintedString);
    intent.setClassName(taintedString, null);
    intent.setData(taintedUri);
    intent.setDataAndNormalize(taintedUri);
    intent.setDataAndType(taintedUri, null);
    intent.setDataAndTypeAndNormalize(taintedUri, null);
    intent.setPackage(taintedString);
    intent.setSelector(taintedIntent);
    intent.setType(taintedString);
    intent.setTypeAndNormalize(taintedString); // 15 sinks, 15 expected reports
  }

}
