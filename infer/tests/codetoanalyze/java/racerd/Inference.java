/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

// test that we can infer when code needs to be thread-safe even without annotations
public class Inference {

  int mField1;

  synchronized void writeUnderLock1Ok() {
    mField1 = 1;
  }

  // should report because there's a write in a method that we think can run on multiple threads
  int unprotectedRead1Bad() {
    int ret = mField1;
    return ret;
  }

  int mField2;

  void writeUnderLock2Ok() {
    synchronized (this) {
      mField2 = 2;
    }
  }

  int unprotectedRead2Bad() {
    int ret = mField2;
    return ret;
  }

  // TODO: handle these casely separately, since assuming that any method whose (transitive) callee
  // uses a lock can run on multiple threads may cause a lot of false positives
  int mField3;

  // empty call that uses a lock
  private synchronized void useLock() {}

  int useLockInCalleeThenReadBad() {
    useLock();
    return mField3;
  }

  void FN_writeToFieldWrittenInLockUsingMethodBad() {
    mField3 = 3;
  }

  int mField4;
  int mField5;

  synchronized int readInsideSyncCoincidentally() {
    mField4 = 4; // we will assume this needs to be protected...
    int ret = mField5; // ...but not this
    return ret;
  }

  int read4OutsideSyncBad() {
    int ret = mField4; // report
    return ret;
  }

  void write5OutsideSyncOk() {
    mField5 = 5; // don't report
  }
}
