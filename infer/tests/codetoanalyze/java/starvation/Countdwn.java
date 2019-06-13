/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;
import java.util.concurrent.CountDownLatch;

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
