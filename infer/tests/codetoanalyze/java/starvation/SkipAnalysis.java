/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.os.Binder;
import android.os.RemoteException;
import android.support.annotation.UiThread;

class SkipAnalysis {
  Binder b;

  void doTransact() throws RemoteException {
    b.transact(0, null, null, 0);
  }

  @UiThread
  void callTransact() throws RemoteException {
    doTransact();
  }
}
