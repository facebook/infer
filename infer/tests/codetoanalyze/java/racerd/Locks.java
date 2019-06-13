/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import javax.annotation.concurrent.ThreadSafe;

@ThreadSafe
public class Locks {
  Integer f;

  Lock mLock;
  ReadWriteLock mReadWriteLock;
  ReentrantLock mReentrantLock;
  ReentrantReadWriteLock mReentrantReadWriteLock;

  // we allow this for now
  public void FN_lockInOneBranchBad(boolean b) {
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

  void nested1Ok() {
    synchronized (this) {
      synchronized (this) {
      }
      // a bad abstraction of locks will treat this as unlocked...
      f = 32;
    }
  }

  void nested2Ok() {
    synchronized (this) {
      synchronized (this) {
        f = 32;
      }
    }
  }

  void nested3Ok() {
    synchronized (this) {
      f = 32;
      synchronized (this) {
      }
    }
  }

  void nested1Bad() {
    synchronized (this) {
      synchronized (this) {
      }
    }
    f = 32;
  }

  void nested2Bad() {
    synchronized (this) {
    }
    f = 32;
    synchronized (this) {
    }
  }

  void nested3Bad() {
    synchronized (this) {
    }
    synchronized (this) {
    }
    f = 32;
  }

  void useLock() {
    synchronized (this) {
    }
  }

  void useLockInCalleeBad() {
    useLock();
    f = 32;
  }

  void lockInLoopOk(int i) {
    while (i > 0) {
      i++;
      mLock.lock();
    }
    f = 32;
  }

  void unlockInLoopOk(int i) {
    mLock.lock();
    while (i > 0) {
      i++;
      mLock.unlock();
    }
    f = 32;
  }

  void lockInLoopLexicalBad(int i) {
    while (i > 0) {
      i++;
      synchronized (this) {
      }
    }
    f = 32;
  }

  void lockInLoopLexicalOk(int i) {
    while (i > 0) {
      i++;
      synchronized (this) {
        f = 32;
      }
    }
  }

  void loopInLockLexicalBad(int i) {
    synchronized (this) {
      while (i > 0) {
        i++;
      }
      f = 32;
    }
  }

  public void unlockOneLockOk() {
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

  boolean mField;

  boolean readUnderLockOk() {
    synchronized (this) {
      return mField;
    }
  }

  void writeUnderLockOk() {
    synchronized (this) {
      mField = true;
    }
  }

  boolean readOutsideLock1Bad() {
    synchronized (this) {
    }
    return mField;
  }

  boolean readOutsideLock2Bad() {
    boolean tmp = mField;
    synchronized (this) {
    }
    return tmp;
  }

  public boolean readInTryCatchWithLockOk() {
    mLock.lock();
    try {
      return mField;
    } finally {
      mLock.unlock();
    }
  }

  public void writeInsideTryCatchWithLockOk() {
    mLock.lock();
    try {
      mField = true;
    } finally {
      mLock.unlock();
    }
  }

  Object mField2;

  private synchronized void lockedWriteInCallee() {
    this.mField2 = null;
  }

  public static void ownedLockedReadOk() {
    Locks owned = new Locks();
    owned.lockedWriteInCallee();
  }

  public Object unownedReadOk() {
    // safe because the only other access to mField is owned
    return this.mField2;
  }

  Object mField3;

  private synchronized void lockedWriteInCallee2() {
    this.mField3 = null;
  }

  public void unownedLockedWriteOk() {
    lockedWriteInCallee2();
  }

  public Object unownedReadBad() {
    return this.mField3;
  }
}
