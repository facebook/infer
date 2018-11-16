/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.accounts.Account;
import android.accounts.AccountManager;
import android.support.annotation.UiThread;

class AccMgr {
  AccountManager am;
  Account account;
  String key, data;
  Object lock;

  // not OK: setUserData may cause stalls, but the general belief is that it's rare
  @UiThread
  void onUiThreadOk() throws InterruptedException {
    am.setUserData(account, key, data);
  }

  // ditto
  @UiThread
  void lockOnUiThreadOk() throws InterruptedException {
    synchronized (lock) {
    }
  }

  void setUserDataUnderLock() {
    synchronized (lock) {
      am.setUserData(account, key, data);
    }
  }

  void onOtherThreadOk() {
    am.setUserData(account, key, data);
  }
}
