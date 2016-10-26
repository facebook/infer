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

class LocallyDefinedException extends RuntimeException {

  public LocallyDefinedException(String message) {
    super(message);
  }

}

public class LocallyDefinedExceptionExample {

  T2 t;

  public LocallyDefinedExceptionExample() {
    this.t = new T2();
    this.t.x = 42;
  }

  void setT(T2 t) {
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
