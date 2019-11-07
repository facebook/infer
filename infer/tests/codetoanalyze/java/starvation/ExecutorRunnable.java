/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.os.Binder;
import android.os.RemoteException;
import java.util.concurrent.Executor;

@interface ForUiThread {}

@interface ForNonUiThread {}

class ExecutorRunnable {
  static Binder binder;
  @ForUiThread private final Executor mUiThreadExecutor = null;
  @ForNonUiThread private final Executor mNonUiThreadExecutor = null;

  private static void doTransact() {
    try {
      binder.transact(0, null, null, 0);
    } catch (RemoteException e) {
    }
  }

  public void FN_postBlockingCallToUIThreadBad() {
    mUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            doTransact();
          }
        });
  }

  public void postBlockingCallToNonUIThreadOk() {
    mNonUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            doTransact();
          }
        });
  }

  Object monitorA, monitorB;

  private void lockAB() {
    synchronized (monitorA) {
      synchronized (monitorB) {
      }
    }
  }

  private void lockBA() {
    synchronized (monitorB) {
      synchronized (monitorA) {
      }
    }
  }

  public void FN_postDeadlockBad() {
    mUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            lockAB();
          }
        });

    mNonUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            lockBA();
          }
        });
  }

  public void postOnUIThreadOk() {
    mUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            lockAB();
          }
        });

    mUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            lockBA();
          }
        });
  }
}
