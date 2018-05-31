/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
