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

  @GuardedBy("this")
  void guardedByThisOk() {
    this.g.toString();
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

}
