/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.util.concurrent.locks.Lock;

class LockHelper {
  Lock lockA, lockD;
  Object lockB, lockC, lockE, lockF;

  void acquireA() {
    lockA.lock();
  }

  // each deadlock should be reported at its own call to acquireA()
  void aThenBBad() {
    acquireA();
    synchronized (lockB) {
    }
    lockA.unlock();
  }

  void bThenABad() {
    synchronized (lockB) {
      lockA.lock();
      lockA.unlock();
    }
  }

  void aThenCBad() {
    acquireA();
    synchronized (lockC) {
    }
    lockA.unlock();
  }

  void cThenABad() {
    synchronized (lockC) {
      lockA.lock();
      lockA.unlock();
    }
  }

  void acquireAD() {
    lockA.lock();
    lockD.lock();
  }

  // the two deadlocks start at different locks taken by the same call to acquireAD(), so neither
  // should suppress the other
  void twoInversionsOneCallBad() {
    acquireAD();
    synchronized (lockE) {
    }
    lockA.unlock();
    synchronized (lockF) {
    }
    lockD.unlock();
  }

  void eThenABad() {
    synchronized (lockE) {
      lockA.lock();
      lockA.unlock();
    }
  }

  void fThenDBad() {
    synchronized (lockF) {
      lockD.lock();
      lockD.unlock();
    }
  }
}
