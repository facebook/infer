/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.app.Activity;
import android.os.Binder;
import android.os.Bundle;
import android.os.RemoteException;

// test is for recognizing Activity lifecycle methods
class MyActivity extends Activity {
  Binder b;

  private void bad() {
    try {
      b.transact(0, null, null, 0);
    } catch (RemoteException r) {
    }
  }

  // overrides so no Bad suffixes

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    bad();
  }

  @Override
  public void onStart() {
    bad();
  }

  @Override
  public void onRestart() {
    bad();
  }

  @Override
  public void onResume() {
    bad();
  }

  @Override
  public void onPause() {
    bad();
  }

  @Override
  public void onStop() {
    bad();
  }

  @Override
  public void onDestroy() {
    bad();
  }
}
