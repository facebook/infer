/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.harness;

import android.app.Activity;

/**
 * If my subclasses null out mObj in an earlier lifecycle method, it will cause
 * a NPE in onDestroy.
 */
public class SuperclassActivity extends Activity {

  Object mObj = new Object();

  @Override
  public void onDestroy() {
    super.onDestroy();
    mObj.toString();
  }
}
