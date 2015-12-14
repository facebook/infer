/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.eradicate;

import android.app.Activity;
import android.os.Bundle;

public class ActivityFieldNotInitialized {

  class BadActivityWithOnCreate extends Activity {

    private String field;

    protected void onCreate(Bundle bundle) {
    }

  }

}
