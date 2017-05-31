/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import android.app.Activity;
import android.content.Intent;
import android.util.Log;

import com.facebook.infer.builtins.InferTaint;

/** Testing that sources and sinks specified in external JSON work correctly */

public class ExternalSpecs {

  // we specify this as a source with kind PrivateData in .inferconfig
  private static Object privateDataSource() {
    return new Object();
  }

  public static void logExternalSourceBad() {
    Log.e("", (String) privateDataSource());
  }

  // we specified that this is a private data source, so passing it an intent sink like
  // startActivity() is fine
  public static void externalSourceAsIntentOk(Activity activity) {
    activity.startActivity((Intent) privateDataSource());
  }

  // we specify that index 1 is an external sink with type Logging in .inferconfig
  public static void loggingSink1(Object notASink, Object sink) {}

  public static void callExternalSinkBad() {
    loggingSink1(null, privateDataSource());
  }

  // passing to non-tainted param
  public static void callExternalSinkOk1() {
    loggingSink1(privateDataSource(), null);
  }

  // passing intent source to logging sink is fine
  public static void callExternalSinkOk2(Activity activity) {
    loggingSink1(null, activity.getIntent());
  }

  // we specify that all the indices are tainted with type Logging in .inferconfig
  public static void loggingSink2(Object sink1, Object sink2) {}

  public static void callExternalSink2Bad1() {
    loggingSink2(privateDataSource(), null);
  }

  public static void callExternalSink2Bad2() {
    loggingSink2(null, privateDataSource());
  }

  // passing intent sources to logging sink is fine
  public static void callExternalSink2Ok(Activity activity) {
    loggingSink2(activity.getIntent(), activity.getIntent());
  }

  static Object sanitizer(Object o) {
    return o;
  }

  void viaSanitizerOk() {
    Object source = InferTaint.inferSecretSource();
    Object sanitized = sanitizer(source);
    InferTaint.inferSensitiveSink(sanitized);
  }

  void sanitizeFootprint(Object o) {
    Object sanitized = sanitizer(o);
    InferTaint.inferSensitiveSink(sanitized);
  }

  void callSanitizeFootprintOk() {
    sanitizeFootprint(InferTaint.inferSecretSource());
  }

  Object returnSanitizedSource() {
    Object source = InferTaint.inferSecretSource();
    return sanitizer(source);
  }

  void callSinkOnSanitizedSourceOk() {
    InferTaint.inferSensitiveSink(returnSanitizedSource());
  }

  Object missedSanitizerBad() {
    Object source = InferTaint.inferSecretSource();
    Object sanitized = sanitizer(source);
    InferTaint.inferSensitiveSink(source);
    return sanitized;
  }


}
