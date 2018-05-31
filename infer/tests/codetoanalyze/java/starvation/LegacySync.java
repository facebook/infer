/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
