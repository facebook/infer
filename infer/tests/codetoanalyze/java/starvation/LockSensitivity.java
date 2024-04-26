/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Tests documenting FPs and FNs due to lack of sensitivity in starvation analysis

import java.util.concurrent.locks.Lock;

class LockSensitivity {
  Lock lockA, lockB;

  // In the following two methods, AB vs BA deadlock pattern
  // remains undetected since one of the locks happens via `tryLock` and result check.

  public void FN_tryLockDeadlockAB_Bad() {
    boolean locked = lockA.tryLock();
    if (locked) {
      lockB.lock();
      lockB.unlock();
      lockA.unlock();
    } else {
    }
  }

  public void FN_tryLockDeadlockBA_Bad() {
    lockB.lock();
    lockA.lock(); // deadlock: `lockA` may be locked via `tryLock()` above
    lockA.unlock();
    lockB.unlock();
  }

  // Asserting a lock is held is not the same as taking it wrt deadlocks.
  // In the following two methods, AB vs BA pattern correctly not reported
  // because the locks/unlocks would not be balanced within the method

  Object monitorA, monitorB;

  public void assertHoldsLockAB_Ok() {
    OurThreadUtils.assertHoldsLock(monitorA);
    OurThreadUtils.assertHoldsLock(monitorB);
  }

  public void assertHoldsLockBA_Ok() {
    OurThreadUtils.assertHoldsLock(monitorB);
    OurThreadUtils.assertHoldsLock(monitorA);
  }
}
