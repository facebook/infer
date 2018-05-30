/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import java.io.FileReader;
import java.util.concurrent.CountDownLatch;
import android.support.annotation.UiThread;
import java.io.IOException;

class Dedup {
  FileReader reader;
  CountDownLatch latch;

  // only one report should be seen
  @UiThread
  void onUiThreadBad() throws InterruptedException, IOException {
    callMethodWithMultipleBlocksBad();
  }

  // three reports are expected
  @UiThread
  void callMethodWithMultipleBlocksBad() throws InterruptedException, IOException  {
    reader.read();
    latch.await();
    reader.read();
  }
}
