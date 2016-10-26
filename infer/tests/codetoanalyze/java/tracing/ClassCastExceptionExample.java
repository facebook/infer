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

class S extends T2 {}

public class ClassCastExceptionExample {

  S cast(T2 t) {
    return (S) t;
  }

  void foo() {
    T2 t = new T2();
    S s = cast(t);
    s.toString();
  }

  T2 m;

  @Verify
  public S bar(int x) {
    if (x < 4 && m != null) {
      return (S) m;
    }
    return null;
  }

}
