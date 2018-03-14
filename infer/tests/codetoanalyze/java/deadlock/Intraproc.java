/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Intraproc {
  void intra_bad(IntraprocA o) {
    synchronized(this) { synchronized(o) {} }
  }

  void intra_ok(IntraprocB o) {
    synchronized(this) { synchronized(o) {} }
  }

  void reentrant_ok(IntraprocB b) {
    synchronized(this) { synchronized(b) { synchronized(this) {} } }
  }
}

class IntraprocA {
  void intra_bad(Intraproc o) {
    synchronized(this) { synchronized(o) {} }
  }
}

class IntraprocB {
  void intra_ok(Intraproc o) {
    synchronized(o) { synchronized(this) {} }
  }
}
