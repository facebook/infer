/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;
import com.facebook.infer.annotation.NonBlocking;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

class NonBlk {
  Future future;

  @NonBlocking
  void doGet() throws InterruptedException, ExecutionException {
    future.get();
  }

  @UiThread
  void onUiThreadIndirectOk() throws InterruptedException, ExecutionException {
    doGet();
  }

  @NonBlocking
  @UiThread
  void onUiThreadDirectOk() throws InterruptedException, ExecutionException {
    future.get();
  }

  @NonBlocking
  synchronized void deadlockABBad() {
    synchronized (future) {
    }
  }

  @NonBlocking
  void deadlockBABad() {
    synchronized (future) {
      synchronized (this) {
      }
    }
  }

  private void privateDoGetOk() throws InterruptedException, ExecutionException {
    future.get();
  }

  @NonBlocking
  @UiThread
  void onUiThreadCalleeOk() throws InterruptedException, ExecutionException {
    privateDoGetOk();
  }
}
