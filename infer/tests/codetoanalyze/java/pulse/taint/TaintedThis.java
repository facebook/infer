/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

public class TaintedThis {
  private String field = "instanceField";

  void taintThisBad() {
    InferTaint.inferSensitiveSink(field);
  }

  void thisIsNotTaintedOk() {
    InferTaint.inferSensitiveSink(field);
  }
}
