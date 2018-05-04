/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import java.util.concurrent.Future;
import java.util.concurrent.ExecutionException;
import android.support.annotation.UiThread;

class FutureGet {
  Future future;
  Object lock;

  @UiThread
  void getDirectBad() throws InterruptedException, ExecutionException {
    future.get();
  }

  @UiThread
  void getIndirectBad() {
    synchronized(lock) {}
  }

  void getUnderLock() throws InterruptedException, ExecutionException {
    synchronized(lock) {
      future.get();
    }
  }

  void getOnOtherThreadOk() throws InterruptedException, ExecutionException {
    future.get();
  }
}
