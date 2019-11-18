/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.os.Binder;
import android.os.RemoteException;
import java.util.concurrent.Executor;

class ModeledExecutors {
  static Binder binder;

  private static void doTransact() {
    try {
      binder.transact(0, null, null, 0);
    } catch (RemoteException e) {
    }
  }

  // starvation via scheduling a transaction on UI thread
  public void postBlockingCallToUIThreadBad() {
    Executors.getForegroundExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }

  public void postBlockingCallToNonUIThreadOk() {
    Executors.getBackgroundExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }
}

// modeled executors
class Executors {
  static Executor uiExecutor;

  static Executor getForegroundExecutor() {
    return uiExecutor;
  }

  static Executor bgExecutor;

  static Executor getBackgroundExecutor() {
    return bgExecutor;
  }
}
