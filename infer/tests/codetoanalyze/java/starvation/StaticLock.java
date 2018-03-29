/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class StaticLock {
  static synchronized void static_synced() {}

  void lock_same_class_one_way_ok() {
    synchronized(StaticLock.class) {
      static_synced();
    }
  }

  static synchronized void lock_same_class_another_way_ok() {
    synchronized(StaticLock.class) {
    }
  }

  void lock_other_class_one_way_bad() {
    synchronized(Object.class) {
      synchronized(this) {}
    }
  }

  synchronized void lock_other_class_another_way_bad() {
    static_synced();
  }
}
