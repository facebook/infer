/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink

object Recursion {

  fun divergeOk() {
    divergeOk()
  }

  fun callSinkThenDiverge(param: Any?) {
    inferSensitiveSink(param)
    callSinkThenDiverge(param)
  }

  fun callSinkThenDivergeBad() {
    callSinkThenDiverge(inferSecretSource())
  }

  fun safeRecursionCallSink(i: Int, param: Any?) {
    if (i == 0) return
    inferSensitiveSink(param)
    safeRecursionCallSink(i - 1, param)
  }

  fun safeRecursionCallSinkBad() {
    safeRecursionCallSink(5, inferSecretSource())
  }

  // TODO (#16595757): Requires support for recursion in Ondemand
  fun FN_recursionBad(i: Int, param: Any?) {
    if (i == 0) return
    inferSensitiveSink(param)
    FN_recursionBad(i - 1, inferSecretSource())
  }
}
