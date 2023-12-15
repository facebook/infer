/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink

class Exceptions {

  @Throws(Exception::class) external fun mayExcept()

  fun FN_sinkInCatchBad1() {
    val source = inferSecretSource()
    try {
      mayExcept()
    } catch (e: Exception) {
      inferSensitiveSink(source)
    }
  }

  fun FN_sinkInCatchBad2() {
    var source: Any? = null
    try {
      source = inferSecretSource()
      mayExcept()
    } catch (e: Exception) {
      inferSensitiveSink(source)
    }
  }

  fun FN_sinkAfterCatchBad() {
    var source: Any? = inferSecretSource()
    try {
      mayExcept()
      source = null
    } catch (e: Exception) {}
    inferSensitiveSink(source)
  }

  fun sinkAfterCatchOk() {
    var source: Any? = inferSecretSource()
    source =
        try {
          mayExcept()
          null
        } catch (e: Exception) {
          null
        }
    inferSensitiveSink(source)
  }

  @Throws(Exception::class)
  fun sinkInFinallyBad1() {
    val source = inferSecretSource()
    try {
      mayExcept()
    } finally {
      inferSensitiveSink(source)
    }
  }

  @Throws(Exception::class)
  fun sinkInFinallyBad2() {
    var source: Any? = null
    try {
      mayExcept()
      source = inferSecretSource()
    } finally {
      inferSensitiveSink(source)
    }
  }

  fun FN_sinkInFinallyBad3() {
    var source: Any? = null
    try {
      mayExcept()
    } catch (e: Exception) {
      source = inferSecretSource()
    } finally {
      inferSensitiveSink(source)
    }
  }

  @Throws(Exception::class)
  fun sinkAfterFinallyOk1() {
    var source: Any? = inferSecretSource()
    try {
      mayExcept()
    } finally {
      source = null
    }
    inferSensitiveSink(source)
  }

  fun sinkAfterFinallyOk2() {
    var source: Any? = null
    try {
      mayExcept()
      source = inferSecretSource()
    } catch (e: Exception) {
      source = inferSecretSource()
    } finally {
      source = null
    }
    inferSensitiveSink(source)
  }

  @Throws(Exception::class)
  fun callSinkThenThrow(param: Any?) {
    inferSensitiveSink(param)
    throw Exception()
  }

  @Throws(Exception::class)
  fun callSinkThenThrowBad() {
    callSinkThenThrow(inferSecretSource())
  }

  @Throws(RuntimeException::class)
  fun doThrow(param: Any) {
    throw RuntimeException(param.toString())
  }

  // see comment on the similar function in codetoanalyze/java/pulse/taint/Exceptions.java
  fun FN_callSinkWithSourceInsideExceptionObjectBad() {
    try {
      doThrow(inferSecretSource())
    } catch (e: RuntimeException) {
      inferSensitiveSink(e)
    }
  }
}
