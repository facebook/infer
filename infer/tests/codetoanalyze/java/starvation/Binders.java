/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import android.os.Binder;
import android.os.RemoteException;
import android.support.annotation.UiThread;

class Binders {
  Binder b;

  void doTransact() throws RemoteException {
    b.transact(0, null, null, 0);
  }

  void doOneWayTransact() throws RemoteException {
    b.transact(0, null, null, 1);
  }

  void interBad() throws RemoteException {
    b.transact(0, null, null, 0);
    forceMainThread();
  }

  void intraBad() throws RemoteException {
    OurThreadUtils.assertMainThread();
    doTransact();
  }

  @UiThread
  void annotationBad() throws RemoteException {
    doTransact();
  }

  void intraOk() throws RemoteException {
    b.transact(0, null, null, 0);
  }

  void interOk() throws RemoteException {
    doTransact();
  }

  void oneWayOk() throws RemoteException {
    OurThreadUtils.assertMainThread();
    doOneWayTransact();
  }

  void forceMainThread() {
    OurThreadUtils.assertMainThread();
  }
}
