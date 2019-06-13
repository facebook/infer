/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Interclass {
  synchronized void interclass1Bad(InterclassA a) {
    a.interclass1Bad();
  }

  synchronized void interclass2Bad() {}

  synchronized void interclass1Ok(InterclassB b) {
    b.interclass1Ok();
  }

  void interclass2Ok(InterclassB b) {
    synchronized (b) {
    }
  }

  void reentrantOk(InterclassB b) {
    synchronized (this) {
      synchronized (b) {
        b.interclass1Ok();
      }
    }
  }
}

class InterclassA {
  synchronized void interclass1Bad() {}

  synchronized void interclass2Bad(Interclass i) {
    i.interclass2Bad();
  }
}

class InterclassB {
  synchronized void interclass1Ok() {}

  void interclass2_ok(Interclass c) {
    synchronized (c) {
      c.interclass2Ok(this);
    }
  }
}
