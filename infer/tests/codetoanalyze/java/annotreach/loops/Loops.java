/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSource {}

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSink {}

class Loops {
  @UserDefinedSink
  void sink() {}

  @UserDefinedSource
  void sourceNoLoopBad() {
    sink();
  }

  @UserDefinedSource
  void sourceWithLoopBad() {
    while (true) {
      sink();
    }
  }

  void forward() {
    sink();
  }

  void forwardWithLoop() {
    while (true) {
      forward();
    }
  }

  @UserDefinedSource
  void sourceForwardBad() {
    forward();
  }

  @UserDefinedSource
  void sourceForwardWithLoopBad() {
    forwardWithLoop();
  }
}
