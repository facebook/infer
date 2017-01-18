/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import javax.annotation.concurrent.ThreadSafe;

class Obj {
  Object f;
  Obj g;
}

@ThreadSafe
public class Ownership {

  Obj field;

  public Ownership(Obj o) {
    field = o;
  }

  native void leakToAnotherThread(Object o);

  public void escapeViaConstructorBad() {
    Obj local = new Obj();
    Ownership constructed = new Ownership(local);
    local.f = new Object();
  }

  public void ownInOneBranchBad(Obj formal, boolean b) {
    if (b) {
      formal = new Obj();
    }
    // we might not own formal
    formal.f = new Object();
  }

  public void reassignToFormalBad(Obj formal) {
    Obj local = new Obj();
    formal.g = local; // bad, we don't own formal
    formal.g.f = new Object(); // ok; now that formal.g is reassigned to local, which we do own
  }

  public void ownedLocalOk() {
    Obj local = new Obj();
    local.f = new Object();
  }

  public Obj returnOwnedLocalOk() {
    Obj local = new Obj();
    local.f = new Object();
    return local;
  }

  public void writeOwnedLocalThenEscapeOk() {
    Obj local = new Obj();
    local.f = new Object();
    leakToAnotherThread(local);
  }

  public void ownInBranchesOk1(boolean b) {
    Obj local;
    if (b) {
      local = new Obj();
      local.f = new Object();
    } else {
      local = new Obj();
      local.f = new Integer(0);
    }
    local.f = new Boolean(false);
  }

  public void ownedAccessPathOk() {
    Obj local = new Obj();
    local.g = new Obj();
    local.g.f = new Object();
  }

  public void aliasOwnedLocalOk() {
    Obj local = new Obj();
    Obj alias = local;
    alias.f = new Object();
    local.f = new Object();
  }

  public void aliasOwnedLocalAccessPathOk() {
    Obj local = new Obj();
    local.g = new Obj();
    Obj alias = local.g;
    alias.f = new Object();
  }

  // we don't understand that ownership has been transferred from returnOwnedLocalOk to the current
  // procedure
  public void FP_ownershipNotInterproceduralOk() {
    Obj local = returnOwnedLocalOk();
    local.f = new Object();
  }

  // we angelically assume that callees don't leak their arguments to another thread for now, so
  // we'll miss this
  public void FN_escapeThenWriteLocalBad() {
    Obj local = new Obj();
    leakToAnotherThread(local);
    local.f = new Object();
  }

}
