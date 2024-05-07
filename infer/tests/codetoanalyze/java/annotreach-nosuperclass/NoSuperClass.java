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
}
