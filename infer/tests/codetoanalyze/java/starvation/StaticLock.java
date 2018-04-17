/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class StaticLock {
  static synchronized void staticSynced() {}

  void lockSameClassOneWayOk() {
    synchronized(StaticLock.class) {
      staticSynced();
    }
  }

  static synchronized void lockSameClassAnotherWayOk() {
    synchronized(StaticLock.class) {
    }
  }

  void lockOtherClassOneWayBad() {
    synchronized(StaticLock.class) {
      synchronized(this) {}
    }
  }

  synchronized void lockOtherClassAnotherWayNad() {
    staticSynced();
  }
}
