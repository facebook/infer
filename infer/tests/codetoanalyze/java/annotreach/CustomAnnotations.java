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
@interface UserDefinedSource1 {}

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSource2 {}

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSink1 {}

@Target({ElementType.METHOD, ElementType.TYPE})
@Retention(RetentionPolicy.CLASS)
@interface UserDefinedSink2 {}

@Target({ElementType.METHOD, ElementType.TYPE})
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

  // This is reported even though it is a superset of an other trace
  @UserDefinedSource2
  void sourceCallsSourceBad() {
    source22Bad();
  }

  @UserDefinedSink1
  void sink1() {}

  @UserDefinedSink2
  void sink2() {}

  @UserDefinedSink1
  void sinkCallsSink() {
    sink1();
  }

  // This is only reported once even though this method ends up calling two sinks
  @UserDefinedSource1
  void sourceCallsSinkThatCallsSinkBad() {
    sinkCallsSink();
  }

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

  void callsSink() {
    sink1();
  }

  @UserDefinedSource1
  void sourceTransitiveCallBad() {
    callsSink();
  }

  @UserDefinedSink1
  @UserDefinedSanitizer
  void sinkAndSanitizer1() {}

  // Order of annotations should not matter
  @UserDefinedSanitizer
  @UserDefinedSink1
  void sinkAndSanitizer2() {}

  @UserDefinedSource1
  void sourceWithSinkAndSanitizer1Ok() {
    sinkAndSanitizer1();
  }

  @UserDefinedSource1
  void sourceWithSinkAndSanitizer2Ok() {
    sinkAndSanitizer2();
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

  void sourceDefinedInConfigOk() {
    safeMethod();
  }

  void sinkDefinedInConfig() {}

  void sourceDefinedInConfigBad() {
    sinkDefinedInConfig();
  }

  void sanitizerDefinedInConfig() {
    sink1();
  }

  @UserDefinedSource1
  void sourceWithSanitizerDefinedInConfigOk() {
    sanitizerDefinedInConfig();
  }

  abstract class Base {
    @UserDefinedSource1
    abstract void sourceBad();

    @UserDefinedSink1
    abstract void sink();

    void safe() {}

    abstract void sourceDefinedInConfigBad();

    abstract void sinkDefinedInConfig();
  }

  class Derived extends Base {
    // Inherits source annotation from base class, should be reported
    @Override
    void sourceBad() {
      sink();
    }

    // Inherits sink annotation from base class
    @Override
    void sink() {}

    void sourceOk() {
      safe();
    }

    // Inherits source (defined in config) from base class, should be reported
    @Override
    void sourceDefinedInConfigBad() {
      sinkDefinedInConfig();
    }

    // Inherits sink (defined in config) from base class
    @Override
    void sinkDefinedInConfig() {}
  }

  void sourceDefinedInConfig_1_WithRegexBad() {
    sinkDefinedInConfig_1_WithRegex();
  }

  void sourceDefinedInConfig_2_WithRegexBad() {
    sinkDefinedInConfig_2_WithRegex();
  }

  void sourceDefinedInConfig_3_WithRegexOk() {
    safeMethod();
  }

  void sinkDefinedInConfig_1_WithRegex() {}

  void sinkDefinedInConfig_2_WithRegex() {}

  @UserDefinedSink1
  interface SinkInterface {
    void interfaceSink();
  }

  @UserDefinedSource1
  class SourceClass implements SinkInterface {
    public void interfaceSink() {}

    void source1Bad() {
      interfaceSink();
    }

    // Inheriting annotation from interface is not disabled so this is a sink
    void sinkBecauseOfTheInterface() {}

    void source2Bad() {
      sinkBecauseOfTheInterface();
    }
  }

  @Deprecated int deprecatedField = 0;

  @Deprecated
  void deprecatedMethod() {}

  public int accessesDeprecatedFieldBad() {
    return deprecatedField;
  }

  public void callsDeprecatedMethodBad() {
    deprecatedMethod();
  }
}
