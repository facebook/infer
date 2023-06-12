/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

import android.app.Activity;
import android.app.Service;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentSender.SendIntentException;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.List;
import org.xmlpull.v1.XmlPullParserException;

class IntentSubclass extends Intent {}

abstract class ContextSubclass extends Context {}

class MyActivity extends Activity {

  @Override
  // intent is modeled as tainted
  public void onActivityResult(int requestCode, int resultCode, Intent intent) {
    startService(intent);
  }

  @Override
  // intent is modeled as tainted
  public void onNewIntent(Intent intent) {
    startService(intent);
  }

  private BroadcastReceiver mReceiver;
  private Uri mUri;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    mReceiver =
        new BroadcastReceiver() {
          @Override
          // intent is modeled as tainted
          public void onReceive(Context context, Intent intent) {
            mUri = intent.getData();
          }
        };
    registerReceiver(mReceiver, null);
  }

  @Override
  public void onResume() {
    startServiceWithTaintedIntent();
  }

  void startServiceWithTaintedIntent() {
    Intent taintedIntent = new Intent("", mUri);
    startService(taintedIntent);
  }
}

class MyBroadcastReceiver extends BroadcastReceiver {

  Activity mActivity;

  @Override
  // intent is modeled as tainted
  public void onReceive(Context context, Intent intent) {
    mActivity.startService(intent);
  }
}

class MyService extends Service {

  Activity mActivity;

  @Override
  // intent is modeled as tainted
  public IBinder onBind(Intent intent) {
    mActivity.startService(intent);
    return null;
  }

  @Override
  // intent is modeled as tainted
  public void onRebind(Intent intent) {
    mActivity.startService(intent);
  }

  @Override
  // intent is modeled as tainted
  public void onStart(Intent intent, int startId) {
    mActivity.startService(intent);
  }

  @Override
  // intent is modeled as tainted
  public int onStartCommand(Intent intent, int flags, int startId) {
    mActivity.startService(intent);
    return 0;
  }

  @Override
  // intent is modeled as tainted
  public void onTaskRemoved(Intent intent) {
    mActivity.startService(intent);
  }

  @Override
  // intent is modeled as tainted
  public boolean onUnbind(Intent intent) {
    mActivity.startService(intent);
    return false;
  }
}

public class Intents {

  private native int rand();

  public void callAllActivitySinksBad(Activity activity)
      throws SendIntentException, IOException, URISyntaxException, XmlPullParserException {
    Uri taintedUri = (Uri) InferTaint.inferSecretSource();
    Intent intent = new Intent("", taintedUri);

    // 20 sinks, 20 expected reports
    activity.bindService(intent, null, 0);
    activity.sendBroadcast(intent);
    activity.sendBroadcastAsUser(intent, null);
    activity.sendOrderedBroadcast(intent, null);
    activity.sendOrderedBroadcastAsUser(intent, null, null, null, null, 0, null, null);
    activity.sendStickyBroadcast(intent);
    activity.sendStickyBroadcastAsUser(intent, null);
    activity.sendStickyOrderedBroadcast(intent, null, null, 0, null, null);
    activity.sendStickyOrderedBroadcastAsUser(intent, null, null, null, 0, null, null);
    activity.startActivities(new Intent[] {intent});
    activity.startActivity(intent);
    activity.startActivityForResult(intent, 0);
    activity.startActivityIfNeeded(intent, 0);
    activity.startActivityFromChild(null, intent, 0);
    activity.startActivityFromFragment(null, intent, 0);
    activity.startIntentSender(null, intent, 0, 0, 0);
    activity.startIntentSenderForResult(null, 0, intent, 0, 0, 0);
    activity.startIntentSenderFromChild(null, null, 0, intent, 0, 0, 0);
    activity.startService(intent);
    activity.stopService(intent);
  }

  public void callAllIntentSinks() throws IOException, URISyntaxException, XmlPullParserException {
    String taintedString = (String) InferTaint.inferSecretSource();
    Intent.parseUri(taintedString, 0);
    Intent.getIntent(taintedString);
    Intent.getIntentOld(taintedString);

    Uri taintedUri = (Uri) InferTaint.inferSecretSource();
    Intent i = new Intent();
    i.setClassName(taintedString, "");
    i.setData(taintedUri);
    i.setDataAndNormalize(taintedUri);
    i.setDataAndType(taintedUri, "");
    i.setDataAndTypeAndNormalize(taintedUri, "");
    i.setPackage(taintedString); // 9 sinks, 9 expected reports
  }

  // make sure the rules apply to subclasses of Intent and Context too
  void subclassCallBad(IntentSubclass intent, ContextSubclass context) {
    String taintedString = (String) InferTaint.inferSecretSource();
    intent.setAction(taintedString);
    context.startActivity(intent);
  }

  void reuseIntentBad(Activity activity) {
    activity.startActivity(activity.getIntent());
  }

  Activity mActivity;

  void extraToDataBad() {
    Intent taintedIntent = (Intent) InferTaint.inferSecretSource();
    String extra = taintedIntent.getStringExtra("foo");

    Intent newIntent1 = new Intent();
    newIntent1.setData(Uri.parse(extra)); // should report
    Intent newIntent2 = new Intent();
    newIntent2.setData(Uri.parse(extra)); // should report
  }

  void extraToExtraBad() {
    Intent taintedIntent = (Intent) InferTaint.inferSecretSource();
    String extra = taintedIntent.getStringExtra("foo");

    Intent newIntent = new Intent();
    newIntent.putExtra("foo", extra);
    mActivity.startActivity(newIntent);
  }

  List<Intent> mIntents;

  Context mContext;

  void callStartWithArrayOk() {
    Intent[] intents = mIntents.toArray(new Intent[mIntents.size()]);
    intents[0] = new Intent(intents[0]).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
    if (startWithArrayOk(mContext, intents)) {
      mContext.startActivity(intents[1]);
    }
  }

  boolean startWithArrayOk(Context context, Intent[] newIntents) {
    context.startActivities(newIntents, null);
    return true;
  }

  void startWithClassLiteralOk() {
    mActivity.startActivity(new Intent(mActivity, MyActivity.class));
  }

  void startWithUri1Bad(Uri uri) {
    mActivity.startActivity(new Intent("action", uri));
  }

  void startWithUri2Bad(Uri uri) {
    mActivity.startActivity(new Intent("action", uri, mActivity, MyActivity.class));
  }
}
