/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

class Constants {

  void source(Object request) {}

  void FP_nullTaintedOk() {
    // TODO: It appears the `null` pointer gets tainted. Same for String
    // literals. While it is probably a configuration error to tell infer
    // that a method can taint a read-only String, the `null` case could
    // lead to some genuine false positives.
    source(null);
    InferTaint.inferSensitiveSink(null);
  }

  void FP_stringLiteralTaintedOk() {
    source("asdf");
    InferTaint.inferSensitiveSink("asdf");
  }
}
