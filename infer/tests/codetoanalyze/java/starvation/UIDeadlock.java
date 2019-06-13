/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;

class UIDeadlock {
  Object lockA, lockB;

  @UiThread
  public synchronized void onUIThreadAOk() {
    synchronized (lockA) {
    }
  }

  @UiThread
  public void onUIThreadBOk() {
    synchronized (lockA) {
      synchronized (this) {
      }
    }
  }

  @UiThread
  public synchronized void onUIThreadBad() {
    synchronized (lockB) {
    }
  }

  public void notOnUIThreadBad() {
    synchronized (lockB) {
      synchronized (this) {
      }
    }
  }
}
