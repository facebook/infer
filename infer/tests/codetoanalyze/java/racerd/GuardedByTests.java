/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.infer;

import com.facebook.infer.annotation.ThreadSafe;
import com.google.common.annotations.VisibleForTesting;
import javax.annotation.concurrent.GuardedBy;

public class GuardedByTests {
  private Object mlock = new Object();

  @GuardedBy("mLock")
  private int a;

  public GuardedByTests() {
    // don't warn on reads or writes of Guarded fields in constructor
    a = 0;
  }

  public void lockedWriteOk() {
    synchronized (mlock) {
      a = 0;
    }
  }

  @GuardedBy("mLock")
  private int b;

  public void unlockedWriteBad() {
    b = 0;
  }

  @GuardedBy("mLock")
  private int c;

  public int unlockedReadOk() {
    return c;
  }

  public int lockedReadOk() {
    synchronized (mlock) {
      return c;
    }
  }

  @GuardedBy("mLock")
  private int d;

  private void privateUnlockedWriteOk() {
    d = 0;
  }

  public void interprocUnlockedWriteBad() {
    privateUnlockedWriteOk();
  }

  @GuardedBy("mLock")
  private int e;

  private int privateUnlockedReadOk() {
    return e;
  }

  public int interprocUnlockedReadOk() {
    return privateUnlockedReadOk();
  }

  @GuardedBy("mLock")
  private int f;

  // NB ThreadSafe annotation disables GuardedBy check too
  @ThreadSafe(enableChecks = false)
  void suppressedWrite() {
    f = 0;
  }

  @GuardedBy("mLock")
  private int h;

  @VisibleForTesting
  public void visibleForTestingOk() {
    h = 0;
  }

  static Object slock = new Object();

  @GuardedBy("slock")
  static int sf;

  static {
    // don't warn on class initializer
    sf = 0;
  }

  @GuardedBy("this")
  int i;

  synchronized void syncWriteOk() {
    i = 5;
  }

  synchronized int syncReadOk() {
    return i;
  }

  GuardedByOther o;

  void accessThroughMemberObjectOk() {
    o.accessBad();
  }

  void accessIndirectOk(GuardedByOther o) {
    o.accessBad();
  }
}

class GuardedByOther {
  @GuardedBy("bla")
  int x;

  void accessBad() {
    x = 0;
  }
}

class GuardedByUiThread {
  @GuardedBy("UiThReAd")
  int a;

  @GuardedBy("ui-thread")
  int b;

  @GuardedBy("UI thread")
  int c;

  @GuardedBy("UI_THREAD")
  int d;

  void uithreadOk() {
    a = b = c = d = 0;
  }
}
