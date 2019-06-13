/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.tracing;

import com.facebook.infer.annotation.Verify;

public class NullPointerExceptionExample {

  void deref(T2 t) {
    t.f();
  }

  @Verify
  void callDeref(T2 t, boolean condition) {
    if (condition) {
      deref(t);
    }
  }

  void callDoesNotLeadToNpe() {
    callDeref(null, false);
  }

  void callLeadToNpe() {
    callDeref(null, true);
  }

  void npeOnBothBranches(int x) {
    if (x < 2) {
      callDeref(null, x < 3);
    } else {
      callDeref(null, true);
    }
  }
}
