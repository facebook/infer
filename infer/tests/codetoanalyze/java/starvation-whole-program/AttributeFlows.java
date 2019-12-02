/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.os.Binder;
import android.os.RemoteException;
import java.util.concurrent.Executor;

class AttributeFlows {
  static Binder binder;

  private static void doTransact() {
    try {
      binder.transact(0, null, null, 0);
    } catch (RemoteException e) {
    }
  }

  private Executor getBackgroundExecutor() {
    return Executors.getBackgroundExecutor();
  }

  private Executor indirectlyGetBackgroundExecutor() {
    return getBackgroundExecutor();
  }

  private Executor getForegroundExecutor() {
    return Executors.getForegroundExecutor();
  }

  private Executor indirectlyGetForegroundExecutor() {
    return getForegroundExecutor();
  }

  // executors are all on background threads, no report
  public void postBlockingCallToForegroundExecutorOk() {
    indirectlyGetForegroundExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }

  // no report here
  public void postBlockingCallToBackgroundExecutorOk() {
    indirectlyGetBackgroundExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }

  @ForUiThread private final Executor mUiThreadExecutor = null;
  @ForNonUiThread private final Executor mNonUiThreadExecutor = null;

  private Executor getAnnotatedBackgroundExecutor() {
    return mNonUiThreadExecutor;
  }

  private Executor indirectlyGetAnnotatedBackgroundExecutor() {
    return getAnnotatedBackgroundExecutor();
  }

  private Executor getAnnotatedForegroundExecutor() {
    return mUiThreadExecutor;
  }

  private Executor indirectlyGetAnnotatedForegroundExecutor() {
    return getAnnotatedForegroundExecutor();
  }

  // starvation via scheduling a transaction on UI thread
  public void postBlockingCallToAnnnotatedUIThreadBad() {
    indirectlyGetAnnotatedForegroundExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }

  // no report here
  public void postBlockingCallToAnnotatedNonUIThreadOk() {
    indirectlyGetAnnotatedBackgroundExecutor()
        .execute(
            new Runnable() {
              @Override
              public void run() {
                doTransact();
              }
            });
  }

  Runnable getBadRunnable() {
    return new Runnable() {
      @Override
      public void run() {
        doTransact();
      }
    };
  }

  public void postRunnableIndirectlyToUIThreadBad() {
    mUiThreadExecutor.execute(getBadRunnable());
  }

  Runnable runnableField =
      new Runnable() {
        @Override
        public void run() {
          doTransact();
        }
      };

  public void postRunnableFieldToUIThreadBad() {
    mUiThreadExecutor.execute(runnableField);
  }
}
