/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink

class Constants {

  fun source(request: Any?) {}

  fun nullNotTaintedOk() {
    source(null)
    InferTaint.inferSensitiveSink(null)
  }

  fun stringLiteralNotTaintedOk() {
    source("asdf")
    InferTaint.inferSensitiveSink("asdf")
  }

  fun nonLiteralTaintedBad() {
    val obj = Any()
    source(obj)
    inferSensitiveSink(obj)
  }
}
