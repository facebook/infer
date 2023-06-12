/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.sinks.InferTaintSinks

/* testing advanced source/sink matchers */
class TaintMatchers {
  fun taintedBasedOnClassNameRegexBad() {
    val src = InferTaint.inferSecretSource()
    InferTaintSinks.sink1(src)
    InferTaintSinks.sink2(src)
  }

  fun notTaintedBasedOnClassNameRegexOk() {
    val src = InferTaint.inferSecretSource()
    InferTaint.sink1(src)
    InferTaint.sink2(src)
  }

  fun taintedFromInferBaseSourceBad() {
    val ics = InferChildSource()
    val source = ics.inferBaseSecretSource()
    InferTaint.inferSensitiveSink(source)
  }

  fun notTaintedFromInferBaseNotSourceGood() {
    val ics = InferChildSource()
    val notSource = ics.inferBaseNotSource()
    InferTaint.inferSensitiveSink(notSource)
  }

  fun taintedFromInferChildSourceBad() {
    val ics = InferChildSource()
    val source = ics.inferChildSecretSource()
    InferTaint.inferSensitiveSink(source)
  }

  fun notTaintedFromInferChildNotSourceGood() {
    val ics = InferChildSource()
    val notSource = ics.inferChildNotSource()
    InferTaint.inferSensitiveSink(notSource)
  }
}
