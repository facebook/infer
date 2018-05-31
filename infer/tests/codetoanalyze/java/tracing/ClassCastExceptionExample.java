/*
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
