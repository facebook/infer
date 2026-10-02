/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import android.support.annotation.UiThread;

class MemberLocksA {
  MemberLocksB b;
  private final Object mLock = new Object();

  // deadlock between lockThenCallBBad() and MemberLocksB.lockThenCallABad()
  void lockThenCallBBad() {
    synchronized (mLock) {
      b.lockB();
    }
  }

  void lockA() {
    synchronized (mLock) {
    }
  }
}

class MemberLocksB {
  MemberLocksA a;
  private final Object mLock = new Object();

  void lockB() {
    synchronized (mLock) {
    }
  }

  void lockThenCallABad() {
    synchronized (mLock) {
      a.lockA();
    }
  }
}

// the monitor of a field is not matched against `this` in the methods of the field's class
class MemberLocksSyncA {
  MemberLocksSyncB b;

  synchronized void FN_callBBad() {
    b.lockB();
  }

  synchronized void lockA() {}
}

class MemberLocksSyncB {
  MemberLocksSyncA a;

  synchronized void lockB() {}

  synchronized void FN_callABad() {
    a.lockA();
  }
}

class MemberLocksNode {
  MemberLocksNode next;
  private final Object mLock = new Object();

  // every method locks a node before the next one
  void lockWithNextOk() {
    synchronized (mLock) {
      synchronized (next.mLock) {
      }
    }
  }
}

class MemberLocksList {
  MemberLocksNode head;

  void lockSecondWithNextOk() {
    head.next.lockWithNextOk();
  }
}

class MemberLocksSlowModel {
  final Object mLock = new Object();

  void sleepUnderLock() throws InterruptedException {
    synchronized (mLock) {
      Thread.sleep(1000);
    }
  }
}

class MemberLocksQueue {
  final Object mLock = new Object();
  boolean mReady;

  void waitUntilReady() throws InterruptedException {
    synchronized (mLock) {
      while (!mReady) {
        mLock.wait();
      }
    }
  }
}

class MemberLocksNestedQueue {
  final Object mLock = new Object();

  void lockThenWaitOnOther(MemberLocksNestedQueue other) throws InterruptedException {
    synchronized (mLock) {
      synchronized (other.mLock) {
        other.mLock.wait();
      }
    }
  }
}

class MemberLocksActivity {
  MemberLocksSlowModel mModel;
  MemberLocksQueue mQueue;
  MemberLocksNestedQueue mNestedQueue;

  @UiThread
  void lockModelBad() {
    synchronized (mModel.mLock) {
    }
  }

  // MemberLocksQueue.waitUntilReady() releases the lock while it waits
  @UiThread
  void notifyQueueOk() {
    synchronized (mQueue.mLock) {
      mQueue.mReady = true;
      mQueue.mLock.notifyAll();
    }
  }

  // MemberLocksNestedQueue.lockThenWaitOnOther() keeps holding its own lock while it waits
  @UiThread
  void lockNestedQueueBad() {
    synchronized (mNestedQueue.mLock) {
    }
  }
}

class MemberLocksUiModel {
  final Object mLock = new Object();

  // MemberLocksWorker.sleepUnderModelLock() may sleep while holding mLock, but the other thread is
  // only searched for in the classes on the paths of the locks of the UI thread
  @UiThread
  void FN_lockOnUiThreadBad() {
    synchronized (mLock) {
    }
  }
}

class MemberLocksWorker {
  MemberLocksUiModel mModel;

  void sleepUnderModelLock() throws InterruptedException {
    synchronized (mModel.mLock) {
      Thread.sleep(1000);
    }
  }
}
