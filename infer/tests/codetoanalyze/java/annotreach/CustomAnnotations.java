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
@interface UserDefinedSink1 {}

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSink2 {}

@Target({ElementType.METHOD})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSanitizer {}

class CustomAnnotations {

  @UserDefinedSource1
  void source11Bad() {
    sink1();
  }

  @UserDefinedSource1
  void source12Bad() {
    sink2();
  }

  @UserDefinedSource2
  void source21Bad() {
    sink1();
  }

  @UserDefinedSource2
  void source22Bad() {
    sink2();
  }

  @UserDefinedSink1
  void sink1() {}

  @UserDefinedSink2
  void sink2() {}

  @UserDefinedSource1
  void source1Ok() {
    safeMethod();
  }

  @UserDefinedSource2
  void source2Ok() {
    safeMethod();
  }

  void safeMethod() {}

  @UserDefinedSource1
  void source1withSanitizerOk() {
    canCallSink();
  }

  @UserDefinedSanitizer
  void canCallSink() {
    sink1();
  }

  interface Callback {
    public void call();
  }

  void caller(Callback lambda) {
    lambda.call();
  }

  @UserDefinedSource1
  void sourceWithLambda1Bad_FN() {
    Callback lambda = () -> sink1();
    lambda.call();
  }

  @UserDefinedSource1
  void sourceWithLambda2Bad_FN() {
    caller(() -> sink1());
  }

  @UserDefinedSource1
  void sourceWithLambda3Ok() {
    Callback lambda = () -> safeMethod();
    lambda.call();
  }

  @UserDefinedSource1
  void sourceWithLambda4Ok() {
    caller(() -> safeMethod());
  }
}
