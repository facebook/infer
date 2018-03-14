/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Interproc {
  synchronized void interproc1_bad(InterprocA a) {
    interproc2_bad(a);
  }

  void interproc2_bad(InterprocA b) {
    synchronized(b) {}
  }

  synchronized void interproc1_ok(InterprocB a) {
    interproc2_ok(a);
  }

  void interproc2_ok(InterprocB b) {
    synchronized(b) {}
  }

  void reentrant1_ok(InterprocB b) {
    synchronized(this) { synchronized(b) { reentrant2_ok(); } }
  }

  synchronized void reentrant2_ok() {}
}

class InterprocA {
  synchronized void interproc1_bad(Interproc c) {
    interproc2_bad(c);
  }

  void interproc2_bad(Interproc d) {
    synchronized(d) {}
  }
}

class InterprocB {
  void interproc1_ok(Interproc c) {
    synchronized(c) { interproc2_ok(c); }
  }

  synchronized void interproc2_ok(Interproc d) {}
}
