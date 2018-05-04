/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

import android.accounts.Account;
import android.accounts.AccountManager;
import android.support.annotation.UiThread;

class AccMgr {
  AccountManager am;
  Account account;
  String key, data;
  Object lock;

  @UiThread
  void onUiThreadBad() throws InterruptedException {
    am.setUserData(account, key, data);
  }

  @UiThread
  void lockOnUiThreadBad() throws InterruptedException {
    synchronized(lock) {}
  }

  void setUserDataUnderLock() {
    synchronized(lock) {
      am.setUserData(account, key, data);
    }
  }

  void onOtherThreadOk() {
    am.setUserData(account, key, data);
  }
}
