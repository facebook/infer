/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

@ThreadSafe
public class Locks {

  Integer f;

  Lock mLock;
  ReadWriteLock mReadWriteLock;
  ReentrantLock mReentrantLock;
  ReentrantReadWriteLock mReentrantReadWriteLock;

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

  public void afterWriteLockUnlockBad() {
    mReentrantReadWriteLock.writeLock().lock();
    mReentrantReadWriteLock.writeLock().unlock();
    f = 42;
  }

  public void lockOk() {
    mLock.lock();
    f = 42;
    mLock.unlock();
  }

  public void lockBothBranchesOk(boolean b) {
    if (b) {
      mLock.lock();
    } else {
      mLock.lock();
    }
    f = 42;
    mLock.unlock();
  }

  public void reentrantLockOk() {
    mReentrantLock.lock();
    f = 42;
    mReentrantLock.unlock();
  }

  public void normalLockTryLockOk() {
    if (mLock.tryLock()) {
      f = 42;
      mLock.unlock();
    }
  }

  public void reentrantLockTryLockOk() {
    if (mReentrantLock.tryLock()) {
      f = 42;
      mReentrantLock.unlock();
    }
  }

  public void tryLockNoCheckBad() {
    mReentrantLock.tryLock(); // might return false
    f = 42;
  }

  public void tryLockWrongBranchBad() {
    if (mReentrantLock.tryLock()) {
    } else {
      f = 42;
    }
  }

  public void tryLockPropagateOk() {
    boolean result = mReentrantLock.tryLock();
    boolean copy = result;
    if (copy) {
      f = 42;
    }
  }

  public void negatedReentrantLockTryLockBad() {
    if (!mReentrantLock.tryLock()) {
      f = 42;
    }
  }

  public void negatedReentrantLockTryLockOk() {
    if (!mReentrantLock.tryLock()) {

    } else {
      f = 42;
    }
  }

  // we could catch this by invalidating the choice predicates whenever we update the lock domain
  public void FN_tryLockStaleBad() {
    boolean result = mReentrantLock.tryLock();
    mReentrantLock.unlock();
    if (result) {
      f = 42; // oops, actually not safe
    }
  }

  public void reentrantLockInterruptiblyOk() throws InterruptedException {
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

  public void writeLockOk() {
    mReadWriteLock.writeLock().lock();
    f = 42;
    mReadWriteLock.writeLock().unlock();
  }

  public void reentrantWriteLockOk() {
    mReentrantReadWriteLock.writeLock().lock();
    f = 42;
    mReentrantReadWriteLock.writeLock().unlock();
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

  // we shouldn't be able to write when holding a readLock
  public void FN_readLockOk() {
    mReentrantReadWriteLock.readLock().lock();
    f = 42;
    mReentrantReadWriteLock.readLock().unlock();
  }

}
