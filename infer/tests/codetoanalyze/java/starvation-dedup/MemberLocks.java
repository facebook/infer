/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class MemberLocksOwner {
  final Object mFirst = new Object();
  final Object mSecond = new Object();

  // the deadlocks of the clients with this method are only found from the clients, as the paths of
  // these locks do not go through the client classes, and are reported whether the name of the
  // client class comes before or after this one
  void lockFirstThenSecond() {
    synchronized (mFirst) {
      synchronized (mSecond) {
      }
    }
  }
}

class AMemberLocksClient {
  MemberLocksOwner mOwner;

  void lockSecondThenFirstBad() {
    synchronized (mOwner.mSecond) {
      synchronized (mOwner.mFirst) {
      }
    }
  }
}

class ZMemberLocksClient {
  MemberLocksOwner mOwner;

  void lockSecondThenFirstBad() {
    synchronized (mOwner.mSecond) {
      synchronized (mOwner.mFirst) {
      }
    }
  }
}

// the deadlock between lockThenCallBBad() and MemberLocksB.lockThenCallABad() is found from both
// sides but reported once
class MemberLocksA {
  MemberLocksB b;
  private final Object mLock = new Object();

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
