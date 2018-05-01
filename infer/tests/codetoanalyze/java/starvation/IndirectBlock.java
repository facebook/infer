/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import android.support.annotation.UiThread;
import android.os.Binder;
import android.os.RemoteException;

class IndirectBlock {
  Object expensiveLock;
  Binder binder;

  void takeExpensiveLockOk() {
    synchronized(expensiveLock) {}
  }

  @UiThread
  void takeExpensiveLockOnUiThreadBad() {
    synchronized(expensiveLock) {}
  }

  void doTransactUnderLock() throws RemoteException {
    synchronized(expensiveLock) {
      binder.transact(0, null, null, 0);
    }
  }

  @UiThread
  void takeRemoteExpensiveLockOnUiThreadBad(IndirectInterproc i) {
    i.takeLock();
  }
}

class IndirectInterproc {
  synchronized public void takeLock() {}

  synchronized public void doTransactUnderLock(Binder binder) throws RemoteException {
    binder.transact(0, null, null, 0);
  }
}
