/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;

class ObjWait {
  Object z;

  void waitOnAnyWithoutTimeoutOk() throws InterruptedException {
    synchronized (z) {
      z.wait();
    }
  }

  Object o;

  @UiThread
  void waitOnMainWithoutTimeoutBad() throws InterruptedException {
    synchronized (o) {
      o.wait();
    }
  }

  @UiThread
  void waitOnMainWithExcessiveTimeout1Bad() throws InterruptedException {
    synchronized (o) {
      o.wait(5001);
    }
  }

  @UiThread
  void waitOnMainWithExcessiveTimeout2Bad() throws InterruptedException {
    synchronized (o) {
      o.wait(4000, 2000000000);
    }
  }

  Object lock, x;

  @UiThread
  void indirectWaitOnMainWithoutTimeoutBad() throws InterruptedException {
    synchronized (lock) {
    }
  }

  void lockAndWaitOnAnyWithoutTimeoutBad() throws InterruptedException {
    synchronized (lock) {
      synchronized (x) {
        x.wait();
      }
    }
  }

  Object y;

  @UiThread
  void indirectWaitSameLockOnMainOk() throws InterruptedException {
    synchronized (y) {
    }
  }

  void lockAndWaitSameLockOnAnyOk() throws InterruptedException {
    synchronized (y) {
      y.wait();
    }
  }
}

class ObjWaitHoldsOtherParam {
  // lockBothWaitOnSecond() keeps holding a while it waits on b
  @UiThread
  void lockOnUiThreadBad() {
    synchronized (this) {
    }
  }

  void lockBothWaitOnSecond(ObjWaitHoldsOtherParam a, ObjWaitHoldsOtherParam b)
      throws InterruptedException {
    synchronized (a) {
      synchronized (b) {
        b.wait();
      }
    }
  }
}

class ObjWaitHoldsParamWaitsOnThis {
  // lockParamWaitOnThis() keeps holding other while it waits on this
  @UiThread
  void lockOnUiThreadBad() {
    synchronized (this) {
    }
  }

  void lockParamWaitOnThis(ObjWaitHoldsParamWaitsOnThis other) throws InterruptedException {
    synchronized (other) {
      synchronized (this) {
        wait();
      }
    }
  }
}

class ObjWaitOnOnlyLock {
  // lockParamWaitOnIt() releases its only lock while it waits
  @UiThread
  void lockOnUiThreadOk() {
    synchronized (this) {
    }
  }

  void lockParamWaitOnIt(ObjWaitOnOnlyLock other) throws InterruptedException {
    synchronized (other) {
      other.wait();
    }
  }
}
