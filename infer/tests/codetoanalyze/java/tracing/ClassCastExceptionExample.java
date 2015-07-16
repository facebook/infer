/*
* Copyright (c) 2015-present Facebook.
* All rights reserved.
*/

package codetoanalyze.java.tracing;

import com.facebook.infer.annotation.Verify;

class S extends T {}

public class ClassCastExceptionExample {

  S cast(T t) {
    return (S) t;
  }

  void foo() {
    T t = new T();
    S s = cast(t);
    s.toString();
  }

  T m;

  @Verify
  public S bar(int x) {
    if (x < 4 && m != null) {
      return (S) m;
    }
    return null;
  }

}
