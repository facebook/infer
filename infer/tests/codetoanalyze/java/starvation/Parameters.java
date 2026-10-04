/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Parameters {
  private static void syncOnParam(Object x) {
    synchronized (x) {
    }
  }

  // Next two methods will deadlock
  public synchronized void oneWaySyncOnParamBad(Object x) {
    syncOnParam(x);
  }

  public void otherWaySyncOnParamBad(Object x) {
    synchronized (x) {
      synchronized (this) {
      }
    }
  }

  private static void emulateSynchronized(Parameters self) {
    synchronized (self) {
    }
  }

  Parameters someObject;

  // Next two methods will deadlock
  public synchronized void oneWayEmulateSyncBad() {
    emulateSynchronized(someObject);
  }

  public void anotherWayEmulateSyncBad() {
    synchronized (someObject) {
      synchronized (this) {
      }
    }
  }

  static Parameters sObject;
  static final Object sLock = new Object();

  // Next two methods will deadlock; the first one has no parameters
  static void staticEmulateSyncBad() {
    synchronized (sLock) {
      emulateSynchronized(sObject);
    }
  }

  void staticObjectThenLockBad() {
    synchronized (sObject) {
      synchronized (sLock) {
      }
    }
  }

  final java.util.concurrent.locks.Lock mLock = new java.util.concurrent.locks.ReentrantLock();

  static Parameters getInstance() {
    return sObject;
  }

  void acquire() {
    mLock.lock();
  }

  void release() {
    mLock.unlock();
  }

  // the lock left held by a method called on an object returned by a call cannot be expressed in
  // the caller, so it is ignored
  static void FN_instanceThenStaticLockBad() {
    getInstance().acquire();
    synchronized (sLock) {
    }
    getInstance().release();
  }

  void FN_staticLockThenInstanceBad() {
    synchronized (sLock) {
      mLock.lock();
      mLock.unlock();
    }
  }
}
