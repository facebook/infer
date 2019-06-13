/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import java.util.concurrent.locks.ReentrantLock;
import javax.annotation.concurrent.ThreadSafe;

class C {
  private int x = 0;

  public int get() {
    return x;
  }

  public void set(int v) {
    x = v;
  }
}

@ThreadSafe
class ReadWriteRaces {

  // read and write outside of sync races
  Integer safe_read;
  Integer racy;

  void m0_OK() {
    Integer local;
    local = safe_read;
  }

  void m0_OK2() { // parallel reads are OK
    Integer local;
    local = safe_read;
  }

  void m1() { // A read where there are other writes
    Integer local;
    local = racy;
  }

  public void m2() {
    racy = 88;
  }

  public void m3() {
    racy = 99;
  }

  // write inside sync, read outside of sync races
  Object field1;
  Object field2;
  Object field3;

  // need to report races involving safe writes in order to get this one
  public synchronized void syncWrite1() {
    field1 = new Object();
  }

  public Object unprotectedRead1() {
    return field1;
  }

  private Object unprotectedReadInCallee() {
    return field1;
  }

  public Object callUnprotecteReadInCallee() {
    return unprotectedReadInCallee();
  }

  public void syncWrite2() {
    synchronized (this) {
      field2 = new Object();
    }
  }

  public Object unprotectedRead2() {
    return field2;
  }

  private synchronized void syncWrite3() {
    field3 = new Object();
  }

  public void callSyncWrite3() {
    syncWrite3();
  }

  public Object unprotectedRead3() {
    return field3;
  }

  private final C c = new C();
  private final ReentrantLock lock = new ReentrantLock();

  public void readInCalleeOutsideSyncBad(int i) {
    if (c.get() > i) { // should report read/write race here
      lock.lock();
      c.set(i);
      lock.unlock();
    }
  }
}
