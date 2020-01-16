/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Interproc {
  synchronized void interproc1Bad(InterprocA a) {
    interproc2(a);
  }

  void interproc2(InterprocA b) {
    synchronized (b) {
    }
  }

  synchronized void interproc1Ok(InterprocB a) {
    interproc2Ok(a);
  }

  void interproc2Ok(InterprocB b) {
    synchronized (b) {
    }
  }

  void reentrant1Ok(InterprocB b) {
    synchronized (this) {
      synchronized (b) {
        reentrant2Ok();
      }
    }
  }

  synchronized void reentrant2Ok() {}
}

class InterprocA {
  synchronized void interproc1Bad(Interproc c) {
    interproc2(c);
  }

  void interproc2(Interproc d) {
    synchronized (d) {
    }
  }
}

class InterprocB {
  void interproc1Ok(Interproc c) {
    synchronized (c) {
      interproc2Ok(c);
    }
  }

  synchronized void interproc2Ok(Interproc d) {}
}
