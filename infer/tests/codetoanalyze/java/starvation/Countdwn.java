/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import java.util.concurrent.CountDownLatch;
import android.support.annotation.UiThread;

class Countdwn {
  CountDownLatch latch;

  void awaitOnMainByCallBad() throws InterruptedException {
    OurThreadUtils.assertMainThread();
    latch.await();
  }

  @UiThread
  void awaitOnMainByAnnotBad() throws InterruptedException {
    latch.await();
  }

  void countDownOk() throws InterruptedException {
    OurThreadUtils.assertMainThread();
    latch.countDown();
  }

  void awaitOnAnyThreadOk() throws InterruptedException {
    latch.await();
  }
}
