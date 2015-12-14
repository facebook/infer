/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.tracing;

import com.facebook.infer.annotation.Verify;

public class NullPointerExceptionExample {

  void deref(T t) {
    t.f();
  }

  @Verify
  void callDeref(T t, boolean condition) {
    if (condition) {
      deref(t);
    }
  }

}
