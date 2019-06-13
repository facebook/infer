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

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSource1 {}

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSource2 {}

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSink {}

class CustomAnnotations {

  @UserDefinedSource1
  void source1Bad() {
    sink();
  }

  @UserDefinedSource2
  void source2Bad() {
    sink();
  }

  @UserDefinedSink
  void sink() {}

  @UserDefinedSource1
  void source1Ok() {
    safeMethod();
  }

  @UserDefinedSource2
  void source2Ok() {
    safeMethod();
  }

  void safeMethod() {}
}
