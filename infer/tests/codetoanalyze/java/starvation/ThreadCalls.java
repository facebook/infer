/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;

class ThreadCalls {
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
    synchronized (lock) {
    }
  }

  void lockAndSleepOnNonUIThread() throws InterruptedException {
    synchronized (lock) {
      sleepOnAnyThreadOk();
    }
  }

  void joinOnAnyThreadOk(Thread thread) throws InterruptedException {
    thread.join();
  }

  @UiThread
  void joinOnUIThreadBad(Thread thread) throws InterruptedException {
    thread.join();
  }

  @UiThread
  void joinWithTimeout1OnUIThreadOk(Thread thread) throws InterruptedException {
    // 50 milliseconds
    thread.join(50);
  }

  @UiThread
  void joinWithTimeout2OnUIThreadOk(Thread thread) throws InterruptedException {
    // 500 milliseconds + 10000 nanoseconds
    thread.join(500, 10000);
  }

  Object joinLock;

  @UiThread
  void indirectJoinOnUIThreadBad() {
    synchronized (joinLock) {
    }
  }

  void lockAndSleepOnNonUIThread(Thread thread) throws InterruptedException {
    synchronized (joinLock) {
      joinOnAnyThreadOk(thread);
    }
  }
}
