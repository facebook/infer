/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink

/** making sure the traces we report respect control-flow */
class FlowSensitivity {

  class Obj {
    var f: Any? = null
  }

  fun callSink(o: Obj) {
    inferSensitiveSink(o.f)
  }

  fun returnSource(o: Obj) {
    o.f = inferSecretSource()
  }

  fun interproceduralFlowSensitivityOk1(o: Obj) {
    inferSensitiveSink(o.f)
    returnSource(o)
  }

  fun interproceduralFlowSensitivityOk2(o: Obj) {
    callSink(o)
    o.f = inferSecretSource()
  }

  fun interproceduralFlowSensitivityOk3(o: Obj) {
    callSink(o)
    returnSource(o)
  }

  fun interproceduralFlowSensitivityBad(o: Obj) {
    returnSource(o)
    callSink(o)
  }

  fun sourceAndSink(o: Obj) {
    inferSensitiveSink(o.f)
    o.f = inferSecretSource()
  }

  fun callSourceAndSinkOk(o: Obj) {
    sourceAndSink(o)
  }

  fun callSourceAndSinkBad1(o: Obj) {
    sourceAndSink(o)
    inferSensitiveSink(o.f)
  }

  fun callSourceAndSinkBad2(o: Obj) {
    o.f = inferSecretSource()
    sourceAndSink(o)
  }
}
