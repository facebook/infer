/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

@Documented
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.CLASS)
@interface ThreadSafe {
}

@ThreadSafe
public class ThreadSafeExample{

  /*Included to make sure infer does not report on class initializers*/
  static Class<?> A = ThreadSafeExample.class;

  Integer f;

  public ThreadSafeExample() {
    f = 86;
  }

  public void tsOK() {
    synchronized (this) {
      f = 42;
    }
  }

  public void tsBad() {
    f = 24;
  }

  Lock mLock;
  ReentrantLock mReentrantLock;

  public void lockInOneBranchBad(boolean b) {
    if (b) {
      mLock.lock();
    }
    f = 24;
    if (b) {
      mLock.unlock();
    }
  }

  public void afterUnlockBad() {
    mLock.lock();
    mLock.unlock();
    f = 42;
  }

  public void afterReentrantLockUnlockBad() {
    mReentrantLock.lock();
    mReentrantLock.unlock();
    f = 42;
  }

  public void withLockOk() {
    mLock.lock();
    f = 42;
    mLock.unlock();
  }

  // shouldn't report here because it's a private method
  private void assignInPrivateMethodOk() {
    f = 24;
  }

  // but should report here, because now it's called
  public void callPublicMethodBad() {
    assignInPrivateMethodOk();
  }

  private void callAssignInPrivateMethod() {
    assignInPrivateMethodOk();
  }

  // should report a deeperTraceBade -> callAssignInPrivateMethod -> assignInPrivateMethodOk trace
  public void deeperTraceBad() {
    callAssignInPrivateMethod();
  }

  public synchronized void callFromSynchronizedPublicMethodOk() {
    assignInPrivateMethodOk();
  }

  private synchronized void synchronizedCallerOk() {
    assignInPrivateMethodOk();
  }

  public void callFromUnsynchronizedPublicMethodOk() {
    synchronizedCallerOk();
  }

  // doesn't work because we don't model lock
  public void FP_tsWithLockOk() {
  }

  public void withLockBothBranchesOk(boolean b) {
    if (b) {
      mLock.lock();
    } else {
      mLock.lock();
    }
    f = 42;
    mLock.unlock();
  }

  public void withReentrantLockOk() {
    mReentrantLock.lock();
    f = 42;
    mReentrantLock.unlock();
  }

  public void withReentrantLockTryLockOk() {
    if (mReentrantLock.tryLock()) {
      f = 42;
      mReentrantLock.unlock();
    }
  }

  public void withReentrantLockInterruptiblyOk() throws InterruptedException {
    mReentrantLock.lockInterruptibly();
    f = 42;
    mReentrantLock.unlock();
  }

  private void acquireLock() {
    mLock.lock();
  }

  public void acquireLockInCalleeOk() {
    acquireLock();
    f = 42;
    mLock.unlock();
  }

  private void releaseLock() {
    mLock.unlock();
  }

  // our "squish all locks into one" abstraction is not ideal here...
  public void FP_unlockOneLock() {
    mLock.lock();
    mReentrantLock.lock();
    mReentrantLock.unlock();
    f = 42;
    mLock.unlock();
  }

  // ... or here
  public void FN_releaseLockInCalleeBad() {
    mLock.lock();
    releaseLock();
    f = 42;
  }

  // we don't model the case where `tryLock` fails
  public void FN_withReentrantLockTryLockBad() {
    if (!mReentrantLock.tryLock()) {
      f = 42;
    }
  }

}

class ExtendsThreadSafeExample extends ThreadSafeExample{

  Integer field;

  /* Presently,we will warn not just on overwridden methods from
  @ThreadSafe class, but potentially on other methods in subclass */
  public void newmethodBad() {
     field = 22;
  }

  /* Bad now that it's overridden */
  public void tsOK() {
     field = 44;
  }

}
