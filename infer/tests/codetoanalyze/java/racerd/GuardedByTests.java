/*
 * Copyright (c) 2016-present, Facebook, Inc.
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
  private int f;

  public GuardedByTests() {
    // don't warn on reads or writes of Guarded fields in constructor
    f = 0;
  }

  public void unlockedWriteBad() {
    f = 0;
  }

  public void lockedWriteOk() {
    synchronized (mlock) {
      f = 0;
    }
  }

  public int unlockedReadBad() {
    return f;
  }

  public int lockedReadOk() {
    synchronized (mlock) {
      return f;
    }
  }

  private void privateUnlockedWriteOk() {
    f = 0;
  }

  private int privateUnlockedReadOk() {
    return f;
  }

  public void interprocUnlockedWriteBad() {
    privateUnlockedWriteOk();
  }

  public int interprocUnlockedReadBad() {
    return privateUnlockedReadOk();
  }

  // NB ThreadSafe annotation disables GuardedBy check too
  @ThreadSafe(enableChecks = false)
  int suppressedRead() {
    return f;
  }

  @VisibleForTesting
  public void visibleForTestingOk() {
    f = 0;
  }

  static Object slock = new Object();

  @GuardedBy("slock")
  static int sf;

  static {
    // don't warn on class initializer
    sf = 0;
  }

  @GuardedBy("this")
  int g;

  synchronized void syncWriteOk() {
    g = 5;
  }

  synchronized int syncReadOk() {
    return g;
  }
}
