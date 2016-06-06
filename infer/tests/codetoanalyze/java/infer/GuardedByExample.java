/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.infer;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

public class GuardedByExample {

  @Retention(RetentionPolicy.CLASS)
  @Target({ElementType.FIELD, ElementType.METHOD})
  public @interface GuardedBy {
    String value();
  }

  private Object mLock = new Object();

  private Object mOtherLock = new Object();

  @GuardedBy("mLock")
  Object f = new Object();

  @GuardedBy("this")
  Object g = new Object();

  Object mCopyOfG;

  @GuardedBy("SomeLockThatDoesntExist")
  Object h = new Object();

  @GuardedBy("ui_thread")
  Object t = new Object();


  void readFBad() {
    this.f.toString();
  }

  void readFBadWrongLock() {
    synchronized (mOtherLock) {
      this.f.toString(); // f is supposed to be protected by mLock
    }
  }

  void readFAfterBlockBad() {
    synchronized (mLock) {
    }
    this.f.toString();
  }

  @GuardedBy("mOtherLock")
  void readFBadWrongAnnotation() {
    this.f.toString();
  }

  @GuardedBy("mLock")
  void readFOkMethodAnnotated() {
    this.f.toString();
  }

  synchronized void synchronizedMethodOk() {
    this.g.toString();
  }

  void readFOkSynchronized() {
    synchronized (mLock) {
      this.f.toString();
    }
  }

  synchronized void synchronizedMethodBad() {
    this.f.toString(); // f is supposed to be protected by mLock, not this
  }

  void readGFromCopyBad() {
    synchronized (this) {
      mCopyOfG = g;  // these are ok: access of g guarded by this
      g.toString();
    }
    mCopyOfG.toString();  // should be an error; unprotected access to pt(g)
  }

  void readHBad() {
    synchronized (mLock) { // h is not protected by mLock
      this.h.toString();
    }
  }

  synchronized void readHBadSynchronizedMethodShouldntHelp() {
    this.h.toString(); // h is not protected by this
  }

  private void privateUnguardedAccess() {
    // not protected, but safe if all call sites guard the access to f
    this.g.toString();
  }

  public void guardedCallSite1() {
    synchronized (this) {
      privateUnguardedAccess(); // should not warn; lock is held
    }
  }

  public synchronized void guardedCallSite2() {
    privateUnguardedAccess(); // should not warn; lock is held
  }

  private void wrapper() {
    privateUnguardedAccess(); // should not warn, just propagate the proof obl
  }

  public void guardedCallSite3() {
    synchronized (this) {
      wrapper(); // should not  warn
    }
  }

  void readTok() {
    this.t.toString();
  }

  // TODO: report on these cases
  /*
  public void unguardedCallSiteBad1() {
    privateUnguardedAccess(); // should warn; lock is not held
  }

  protected void unguardedCallSiteBad2() {
    privateUnguardedAccess(); // should warn; lock is not held
  }

  void unguardedCallSiteBad3() {
    privateUnguardedAccess(); // should warn; lock is not held
  }
  */

}
