/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;
import java.lang.Thread;

class ThreadSleep {
  void sleepOnAnyThreadOk() throws InterruptedException {
    Thread.sleep(60);
  }

  @UiThread
  void sleepOnUIThreadBad() throws InterruptedException {
    Thread.sleep(60);
  }

  Object lock;

  @UiThread
  void indirectSleepOnUIThreadBad() {
    synchronized(lock) {}
  }

  void lockAndSleepOnNonUIThread() throws InterruptedException {
    synchronized(lock) {
      sleepOnAnyThreadOk();
    }
  }

}
