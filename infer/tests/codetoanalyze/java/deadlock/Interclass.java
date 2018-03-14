/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

class Interclass {
  synchronized void interclass1_bad(InterclassA a) {
    a.interclass1_bad();
  }

  synchronized void interclass2_bad() {}

  synchronized void interclass1_ok(InterclassB b) {
    b.interclass1_ok();
  }

  void interclass2_ok(InterclassB b) {
    synchronized(b) {}
  }

  void reentrant_ok(InterclassB b) {
    synchronized(this) { synchronized(b) { b.interclass1_ok(); } }
  }
}

class InterclassA {
  synchronized void interclass1_bad() {}

  synchronized void interclass2_bad(Interclass i) {
    i.interclass2_bad();
  }
}

class InterclassB {
  synchronized void interclass1_ok() {}

  void interclass2_ok(Interclass c) {
    synchronized(c) { c.interclass2_ok(this); }
  }
}
