/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class MemberLocksTask implements Runnable {
  final Object mLock = new Object();
  MemberLocksOtherTask mOther;

  @Override
  public void run() {
    synchronized (mLock) {
      synchronized (mOther.mLock) {
      }
    }
  }
}

class MemberLocksOtherTask implements Runnable {
  final Object mLock = new Object();
  MemberLocksTask mTask;

  @Override
  public void run() {
    synchronized (mLock) {
      synchronized (mTask.mLock) {
      }
    }
  }
}

class MemberLocksMain {
  // the two tasks lock each other's lock last
  public static void main(String args[]) {
    Thread t1 = new Thread(new MemberLocksTask());
    t1.start();
    Thread t2 = new Thread(new MemberLocksOtherTask());
    t2.start();
  }
}
