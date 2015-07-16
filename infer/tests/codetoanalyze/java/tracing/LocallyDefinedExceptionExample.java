/*
* Copyright (c) 2015-present Facebook.
* All rights reserved.
*/


package codetoanalyze.java.tracing;

import com.facebook.infer.annotation.Verify;

class LocallyDefinedException extends RuntimeException {

  public LocallyDefinedException(String message) {
    super(message);
  }

}

public class LocallyDefinedExceptionExample {

  T t;

  public LocallyDefinedExceptionExample() {
    this.t = new T();
    this.t.x = 42;
  }

  void setT(T t) {
    this.t = t;
  }

  @Verify
  void fieldInvariant() {
    if (this.t != null) {
      if (this.t.x != 42) {
        throw new LocallyDefinedException("Field expected to be equal to 42");
      } else {
        this.t.x = 0;
      }
    }
  }

}
