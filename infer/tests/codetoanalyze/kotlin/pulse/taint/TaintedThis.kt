/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class TaintedThis {
  private val field = "instanceField"

  fun taintThisBad() {
    InferTaint.inferSensitiveSink(field)
  }

  fun thisIsNotTaintedOk() {
    InferTaint.inferSensitiveSink(field)
  }
}
