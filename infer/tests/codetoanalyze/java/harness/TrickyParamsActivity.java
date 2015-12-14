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

/*
 * Test if type inhabitation can inhabit tricky types.
 */
public class TrickyParamsActivity extends Activity {

  private Object mObj;

  // we have to be able to inhabit all params or no methods from this
  // class will be called by the harness
  public TrickyParamsActivity(
      Object o, int i, boolean b, char c, long l,
      float f, double d, short sh, byte byt,
      Object[] arr1, int[] arr2) {
    this.mObj = new Object();
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
