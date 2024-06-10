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

class NoSuperClass {
  @UserDefinedSink
  interface SinkInterface {
    void interfaceSink();
  }

  @UserDefinedSource
  class SourceClass implements SinkInterface {
    // This inherits sink annotation because the method comes from the interface
    public void interfaceSink() {}

    void source1Bad() {
      interfaceSink();
    }

    // This does not inherit sink annotation because it does not come from the interface
    void notSink() {}

    void source2Ok() {
      notSink();
    }
  }

  class DerivedClass extends SourceClass {
    // This inherits source annotation from superclass because the method overrides the
    // method from the superclass
    @Override
    void source1Bad() {
      interfaceSink();
    }

    // This does not inherit source annotation from superclass because
    // it does not come from the superclass
    void notSource() {
      interfaceSink();
    }
  }

  @UserDefinedSink
  @UserDefinedSource
  void sourceAndSinkBad() {}
}

class Minimize {
  @UserDefinedSink
  void finalSink() {}

  @UserDefinedSink
  void sinkCallingSink() {
    finalSink();
  }

  @UserDefinedSource
  void sourceCallingSinkBad() {
    sinkCallingSink();
  }

  // This is not reported due to minimization
  @UserDefinedSource
  void sourceCallingSourceOk() {
    sourceCallingSinkBad();
  }
}
