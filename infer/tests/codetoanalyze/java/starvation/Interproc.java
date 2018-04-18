/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Interproc {
  synchronized void interproc1Bad(InterprocA a) {
    interproc2Bad(a);
  }

  void interproc2Bad(InterprocA b) {
    synchronized(b) {}
  }

  synchronized void interproc1Ok(InterprocB a) {
    interproc2Ok(a);
  }

  void interproc2Ok(InterprocB b) {
    synchronized(b) {}
  }

  void reentrant1Ok(InterprocB b) {
    synchronized(this) {
      synchronized(b) {
        reentrant2Ok();
      }
    }
  }

  synchronized void reentrant2Ok() {}
}

class InterprocA {
  synchronized void interproc1Bad(Interproc c) {
    interproc2Bad(c);
  }

  void interproc2Bad(Interproc d) {
    synchronized(d) {}
  }
}

class InterprocB {
  void interproc1Ok(Interproc c) {
    synchronized(c) {
      interproc2Ok(c);
    }
  }

  synchronized void interproc2Ok(Interproc d) {}
}
