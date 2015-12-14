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
import android.view.View;
import android.widget.Button;

/*
 * Test if harness generation knows how to call a callback defined in an inner class
 */
public class CallbackActivity extends Activity {

  private Object mObj;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    Button btn = new Button(this.getApplicationContext());
    mObj = new Object();
    Button.OnClickListener listener = new Button.OnClickListener() {

      @Override
      public void onClick(View v) {
        // oops! what if I get nulled out later?
        mObj.toString();
      }
    };
    btn.setOnClickListener(listener);
    mObj = null;
  }

}
