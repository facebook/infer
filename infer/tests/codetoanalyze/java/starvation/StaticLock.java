/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class StaticLock {
  static synchronized void staticSynced() {}

  void lockSameClassOneWayOk() {
    synchronized (StaticLock.class) {
      staticSynced();
    }
  }

  static synchronized void lockSameClassAnotherWayOk() {
    synchronized (StaticLock.class) {
    }
  }

  void lockOtherClassOneWayBad() {
    synchronized (StaticLock.class) {
      synchronized (this) {
      }
    }
  }

  synchronized void lockOtherClassAnotherWayBad() {
    staticSynced();
  }
}
