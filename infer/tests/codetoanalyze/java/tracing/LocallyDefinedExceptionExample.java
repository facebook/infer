/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
