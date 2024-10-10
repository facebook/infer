/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.annotreach;

// Testing when Kotlin annotations are used in Java code
class CustomAnnotationsJava {
  @UserDefinedSink
  void sink() {}

  @UserDefinedSanitizer
  void canCallSink() {
  }

  @UserDefinedSource
  void source1Bad() {
    sink();
  }

  @UserDefinedSource
  void source2Ok() {
    canCallSink();
  }
}
