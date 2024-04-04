/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

import codetoanalyze.kotlin.pulse.InferTaint.inferSecretSource
import codetoanalyze.kotlin.pulse.InferTaint.inferSensitiveSink
import java.util.*

/**
 * a lot of tainted values are strings, so propagation through StringBuilder's and the like is very
 * important.
 */
class Strings {

  fun viaStringBuilderSugarBad() {
    val source = inferSecretSource()
    inferSensitiveSink(source.toString() + "")
  }

  fun viaStringBuilderBad() {
    val source = inferSecretSource()
    val builder = StringBuilder()
    inferSensitiveSink(builder.append(source).append("").toString())
  }

  fun viaStringBuilderIgnoreReturnBad() {
    val source = inferSecretSource()
    val builder = StringBuilder()
    // builder should be tainted after this call even though we ignore the return value
    builder.append(source)
    inferSensitiveSink(builder.toString())
  }

  fun viaStringBufferBad() {
    val source = inferSecretSource()
    val buffer = StringBuffer()
    inferSensitiveSink(buffer.append("").append(source).toString())
  }

  fun viaStringBufferIgnoreReturnBad() {
    val source = inferSecretSource()
    val buffer = StringBuffer()
    buffer.append(source)
    inferSensitiveSink(buffer.toString())
  }

  fun viaFormatterBad() {
    val source = inferSecretSource()
    val formatter = Formatter()
    inferSensitiveSink(formatter.format("%s", source).toString())
  }

  fun viaFormatterIgnoreReturnBad() {
    val source = inferSecretSource()
    val formatter = Formatter()
    formatter.format("%s", source)
    inferSensitiveSink(formatter.toString())
  }

  fun viaStringFormatVarArgsDirectBad() {
    val source = inferSecretSource()
    val tainted = String.format("%s%s", "hi", source)
    inferSensitiveSink(tainted)
  }

  fun viaStringFormatVarArgsIndirect(param: Any?) {
    val tainted = String.format("%s%s", "hi", param)
    inferSensitiveSink(tainted)
  }

  fun viaStringFormatVarArgsIndirectBad() {
    viaStringFormatVarArgsIndirect(inferSecretSource())
  }
}
