/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class LocalLock {
  Object lock1, lock2;

  Object get_lock1() {
    return lock1;
  }

  void local_lock_outer_ok() {
    synchronized(get_lock1()) {
      synchronized(this) {}
    }
  }

  Object get_lock2() {
    return lock2;
  }

  void local_lock_inner_ok() {
    synchronized(this) {
      synchronized(get_lock2()) {}
    }
  }
}
