/*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.eradicate;

import android.app.Activity;
import android.os.Bundle;

public class ActivityFieldNotInitialized {

  class BadActivityWithOnCreate extends Activity {

    private String field;

    protected void onCreate(Bundle bundle) {}
  }
}
