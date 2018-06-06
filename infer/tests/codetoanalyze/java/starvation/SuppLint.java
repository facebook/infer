/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;
import java.io.IOException;
import android.support.annotation.UiThread;
import android.annotation.SuppressLint;

class SuppLint {
  Future future;

  @UiThread
  @SuppressLint("starvation")
  void onUiThreadSuppressed() throws InterruptedException, ExecutionException {
    future.get();
  }

  @UiThread
  @SuppressLint("someOtherString")
  void onUiThreadBad() throws InterruptedException, ExecutionException {
    future.get();
  }
}

@SuppressLint("STARVATION")
class SuppLintClass {
  Future future;
  @UiThread
  void onUiThreadSuppressed() throws InterruptedException, ExecutionException {
    future.get();
  }
}
