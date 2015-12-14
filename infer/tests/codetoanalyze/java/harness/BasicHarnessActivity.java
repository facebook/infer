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
import android.os.Bundle;

/*
 * Test if harness generation understands basics of Activity lifecycle.
 */
public class BasicHarnessActivity extends Activity {

  public BasicHarnessActivity(BasicHarnessActivity a) {
  }

  private Object mObj;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    mObj = new Object();
  }

  @Override
  public void onPause() {
    mObj = null;
  }

  @Override
  public void onDestroy() {
    String s = mObj.toString();
  }

}
