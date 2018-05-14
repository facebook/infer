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
import java.util.Hashtable;

class LegacySync {
  Hashtable table;
  Future future;

  void notOnUiThreadSyncedBad() throws InterruptedException, ExecutionException {
    synchronized(table) {
      future.get();
    }
  }

  @UiThread
  Object onUiThreadOpBad() {
     return table.get("blabla");
  }
}
