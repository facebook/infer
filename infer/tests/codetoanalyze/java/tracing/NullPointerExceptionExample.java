// Copyright (c) 2015-Present Facebook. All rights reserved.

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
