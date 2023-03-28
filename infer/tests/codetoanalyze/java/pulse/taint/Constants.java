/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

class Constants {

  void source(Object request) {}

  void nullNotTaintedOk() {
    source(null);
    InferTaint.inferSensitiveSink(null);
  }

  void stringLiteralNotTaintedOk() {
    source("asdf");
    InferTaint.inferSensitiveSink("asdf");
  }

  void nonLiteralTaintedBad() {
    Object obj = new Object();
    source(obj);
    InferTaint.inferSensitiveSink(obj);
  }
}
