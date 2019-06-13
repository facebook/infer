/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package infer.other;

import android.annotation.SuppressLint;
import android.os.Bundle;
import android.support.v7.app.ActionBarActivity;

public class MainActivity extends ActionBarActivity {

  Object source() {
    return null;
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    source().toString();
  }

  @SuppressLint("NULL_DEREFERENCE")
  void shouldNotBeReported() {
    source().toString();
  }
}
