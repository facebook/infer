/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import android.os.IBinder;
import android.content.Intent;
import android.app.Service;
import android.app.IntentService;
import android.os.RemoteException;

class ServiceOnUIThread extends Service {
  private final IBinder mBinder = null;

  @Override
  public IBinder onBind(Intent intent) {
    transactBad();
    return mBinder;
  }

  void transactBad() {
    try {
      mBinder.transact(0, null, null, 0);
    } catch (RemoteException e) {}
  }
}

class IntentServiceNotOnUIThread extends IntentService {
  private final IBinder mBinder = null;

  public IntentServiceNotOnUIThread() {
    super("Intent service");
  }

  @Override
  public void onHandleIntent(Intent intent) {
    transactOk();
  }

  void transactOk() {
    try {
      mBinder.transact(0, null, null, 0);
    } catch (RemoteException e) {}
  }
}
