/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Interproc {
  synchronized void lockThisThenParamBad(InterprocA a) {
    lockParamA(a);
  }

  void lockParamA(InterprocA b) {
    synchronized (b) {
    }
  }

  synchronized void lockThisThenParamOk(InterprocB a) {
    lockParamB(a);
  }

  void lockParamB(InterprocB b) {
    synchronized (b) {
    }
  }

  void lockThisTwiceOk(InterprocB b) {
    synchronized (this) {
      synchronized (b) {
        lockThis();
      }
    }
  }

  synchronized void lockThis() {}
}

class InterprocA {
  synchronized void lockThisThenParamBad(Interproc c) {
    lockParam(c);
  }

  void lockParam(Interproc d) {
    synchronized (d) {
    }
  }
}

class InterprocB {
  void lockParamThenThisOk(Interproc c) {
    synchronized (c) {
      lockThis(c);
    }
  }

  synchronized void lockThis(Interproc d) {}
}
