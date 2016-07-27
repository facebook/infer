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
import com.google.common.annotations.VisibleForTesting;

import java.io.Closeable;

public class GuardedByExample {

  @Retention(RetentionPolicy.CLASS)
  @Target({ElementType.FIELD, ElementType.METHOD})
  public @interface GuardedBy {
    String value();
  }

  static class AutoCloseableReadWriteUpdateLock implements Closeable {
    @Override public void close() {}
  }

  private Object mLock = new Object();

  private Object mOtherLock = new Object();

  private AutoCloseableReadWriteUpdateLock mReadWriteLock = new AutoCloseableReadWriteUpdateLock();

  @GuardedBy("mLock")
  private Object f = new Object();

  @GuardedBy("this")
  Object g = new Object();

  Object mCopyOfG;

  @GuardedBy("SomeLockThatDoesntExist")
  Object h = new Object();

  @GuardedBy("mReadWriteLock")
  Object i = new Object();

  private static Object sLock = new Object();

  @GuardedBy("sLock")
  static Object sFld;

  @GuardedBy("GuardedByExample.class")
  static Object sGuardedByClass;

  static {
    // don't warn on class initializer
    sFld = new Object();
  }

  public GuardedByExample() {
    // don't warn on reads or writes of Guarded fields in constructor
    f.toString();
    g = new Object();
  }

  void readFBad() {
    this.f.toString();
  }

  void writeFBad() {
    this.f = new Object();
  }

  void readFBadWrongLock() {
    synchronized (mOtherLock) {
      this.f.toString(); // f is supposed to be protected by mLock
    }
  }

  void writeFBadWrongLock() {
    synchronized (mOtherLock) {
      this.f = new Object(); // f is supposed to be protected by mLock
    }
  }

  void readFAfterBlockBad() {
    synchronized (mLock) {
    }
    this.f.toString();
  }

  void writeFAfterBlockBad() {
    synchronized (mLock) {
    }
    this.f = new Object();
  }

  @GuardedBy("mOtherLock")
  void readFBadWrongAnnotation() {
    this.f.toString();
  }

  @GuardedBy("mLock")
  void readFOkMethodAnnotated() {
    this.f.toString();
  }

  synchronized void synchronizedMethodReadOk() {
    this.g.toString();
  }

  synchronized void synchronizedMethodWriteOk() {
    this.g = new Object();
  }

  void readFOkSynchronized() {
    synchronized (mLock) {
      this.f.toString();
    }
  }

  void writeFOkSynchronized() {
    synchronized (mLock) {
      this.f = new Object();
    }
  }

  synchronized void synchronizedMethodReadBad() {
    this.f.toString(); // f is supposed to be protected by mLock, not this
  }

  synchronized void synchronizedMethodWriteBad() {
    this.f = new Object(); // f is supposed to be protected by mLock, not this
  }

  void reassignCopyOk() {
    synchronized (this) {
      mCopyOfG = g;  // these are ok: access of g guarded by this
    }
    mCopyOfG = new Object(); // ok; this doesn't change the value of g
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

  void readWriteLockOk() {
    try (AutoCloseableReadWriteUpdateLock lock = mReadWriteLock) {
      this.i.toString();
    }
  }

  synchronized static void staticSynchronizedOk() {
    sGuardedByClass.toString();
  }

  static void synchronizeOnClassOk() {
    synchronized(GuardedByExample.class) {
      sGuardedByClass.toString(); // should not warn here
    }
  }

  void synchronizedOnThisBad() {
    sGuardedByClass.toString();
  }
  
  Object dontReportOnCompilerGenerated() {
    return new Object() {
      public void accessInAnonClassOk() {
        synchronized (mLock) {
          f.toString();
        }
      }
    };
  }

  Object readFromInnerClassOkOuter() {
    return new Object() {
      public String readFromInnerClassOk() {
        synchronized (GuardedByExample.this) {
          return g.toString();
        }
      }
    };
  }

  Object readFromInnerClassBad1Outer() {
    return new Object() {
      public String readFromInnerClassBad1() {
        synchronized (this) {
          return g.toString(); // g is guarded by the outer class this, not this$0
        }
      }
    };
  }

  Object readFromInnerClassBad2Outer() {
    return new Object() {
      public synchronized String readFromInnerClassBad2() {
        return g.toString(); // g is guarded by the outer class this, not this$0
      }
    };
  }

  @VisibleForTesting
  public void visibleForTestingOk1() {
    f.toString(); // should push proof obl to caller
  }

  @VisibleForTesting
  void visibleForTestingOk2() {
    f.toString(); // should push proof obl to caller
  }

  synchronized Object returnPtG() {
    return g;
  }

  // note: this test should raise an error under "by value" GuardedBy semantics, but not under
  // "by reference" GuardedBy semantics
  void readGFromCopyOk() {
    synchronized (this) {
      mCopyOfG = g;  // these are ok: access of g guarded by this
      g.toString();
    }
    mCopyOfG.toString();  // should be an error; unprotected access to pt(g)
  }

  // another "by reference" vs "by value" test. buggy in "by value", but safe in "by reference"
  void usePtG() {
    Object ptG = returnPtG();
    ptG.toString();
  }

  Object byRefTrickyBad() {
    Object local = null;
    synchronized(this) {
      local = g; // we have a local pointer... to pt(G)
    }
    g.toString(); // ...but unsafe access is through g!
    return local;
  }

  void byRefTrickyOk() {
    Object local = null;
    synchronized(this) {
      local = g; // we have a local pointer... to pt(G)
    }
    local.toString(); // ...but unsafe access is through g!
  }


  @GuardedBy("ui_thread")
  Object uiThread1;
  @GuardedBy("ui-thread")
  Object uiThread2;
  @GuardedBy("uithread")
  Object uiThread3;

  @GuardedBy("something that's clearly not an expression")
  Object nonExpression;

  // tests for not reporting false alarms on unrecognized GuardedBy strings
  void accessUnrecognizedGuardedByFieldsOk() {
    uiThread1 = new Object();
    uiThread1.toString();
    uiThread2 = new Object();
    uiThread2.toString();
    uiThread3 = new Object();
    uiThread3.toString();
    nonExpression = new Object();
    nonExpression.toString();
  }

  // outer class this tests
  @GuardedBy("GuardedByExample.this")
  Object guardedByOuterThis;

  synchronized void okOuterAccess() {
    guardedByOuterThis = null;
  }

  // inner class this tests
  private class Inner {
    @GuardedBy("this")
    Object guardedByInnerThis1;
    @GuardedBy("Inner.this")
    Object guardedByInnerThis2;
    @GuardedBy("GuardedByExample$Inner.this")
    Object guardedByInnerThis3;

    synchronized void okAccess1() {
      guardedByInnerThis1 = null;
    }

    synchronized void okAccess2() {
      guardedByInnerThis2 = null;
    }

    synchronized void okAccess3() {
      guardedByInnerThis3 = null;
    }
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
