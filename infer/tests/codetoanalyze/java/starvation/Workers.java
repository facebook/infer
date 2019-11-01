/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.os.Binder;
import android.os.RemoteException;
import android.support.annotation.UiThread;
import android.support.annotation.WorkerThread;

class Workers {
  Binder b;

  void doTransact() throws RemoteException {
    b.transact(0, null, null, 0);
  }

  @WorkerThread
  void workerOk() throws RemoteException {
    doTransact();
  }

  // WorkerThread does not propagate up the call stack
  // We don't report here, however, since this is an annotation/assertion error,
  // not starvation/deadlock.
  @UiThread
  void FN_uiThreadBad() throws RemoteException {
    workerOk();
  }

  // WorkerThread wins
  @WorkerThread
  @UiThread
  void bothOk() throws RemoteException {
    workerOk();
  }
}
