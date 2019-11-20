/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.os.Binder;
import android.os.RemoteException;
import java.util.concurrent.Executor;

// we treat executors of unknown thread as implicitly running in the background

class UnknownThread {
  static Binder binder;

  private static void doTransact() {
    try {
      binder.transact(0, null, null, 0);
    } catch (RemoteException e) {
    }
  }

  @ForUiThread private final Executor mUiThreadExecutor = null;
  @ForNonUiThread private final Executor mNonUiThreadExecutor = null;
  Executor unknownThreadExecutor = null;

  private static Executor getSomeExecutor() {
    return null;
  }

  public void postBlockingCallToUnknownExecutorFieldOk() {
    unknownThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            doTransact();
          }
        });
  }

  public void postBlockingCallToUnknownExecutorViaMethodOk() {
    getSomeExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }

  Object monitorA, monitorB;

  // text-book deadlock between unknown and background thread
  public void postDeadlockToUnknownAndBackgroundBad() {
    unknownThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            synchronized (monitorA) {
              synchronized (monitorB) {
              }
            }
          }
        });

    mNonUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            synchronized (monitorB) {
              synchronized (monitorA) {
              }
            }
          }
        });
  }

  Object monitorC, monitorD;

  // text-book deadlock between unknown and background thread
  public void postDeadlockToUIAndBackgroundBad() {
    unknownThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            synchronized (monitorC) {
              synchronized (monitorD) {
              }
            }
          }
        });

    mUiThreadExecutor.execute(
        new Runnable() {
          @Override
          public void run() {
            synchronized (monitorD) {
              synchronized (monitorC) {
              }
            }
          }
        });
  }
}
