/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

/** testing basic intraprocedural functionality: assignment, ifs, loops, casts */
class Basics {

  external fun notASource(): Any

  external fun notASink(o: Any)

  /** should report on these tests */
  fun directBad() {
    InferTaint.inferSensitiveSink(InferTaint.inferSecretSource())
  }

  fun viaVarBad1() {
    val src = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(src)
  }

  fun viaVarBad2() {
    val src = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(src)
  }

  fun viaVarBad3() {
    var src: Any? = InferTaint.inferSecretSource()
    val alias = src as Any
    src = null
    InferTaint.inferSensitiveSink(alias)
  }

  fun viaCastBad1() {
    InferTaint.inferSensitiveSink((InferTaint.inferSecretSource() as String))
  }

  fun viaCastBad2() {
    val src = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink((src as String))
  }

  fun ifBad1(b: Boolean) {
    var src: Any? = null
    if (b) {
      src = InferTaint.inferSecretSource()
    }
    InferTaint.inferSensitiveSink(src)
  }

  fun ifBad2(b: Boolean) {
    var src: Any? = InferTaint.inferSecretSource()
    if (b) {
      src = null
    }
    InferTaint.inferSensitiveSink(src)
  }

  fun ifBad3(b: Boolean) {
    val src: Any =
        if (b) {
          Any()
        } else {
          InferTaint.inferSecretSource()
        }
    InferTaint.inferSensitiveSink(src)
  }

  fun ifBad4(b1: Boolean, b2: Boolean) {
    val src: Any? =
        if (b1) {
          Any()
        } else if (b2) {
          InferTaint.inferSecretSource()
        } else {
          null
        }
    InferTaint.inferSensitiveSink(src)
  }

  fun ifBad5(b: Boolean) {
    val src = InferTaint.inferSecretSource()
    if (b) {
      InferTaint.inferSensitiveSink(src)
    }
  }

  fun switchBad1(i: Int) {
    val src = InferTaint.inferSecretSource()
    when (i) {
      1 -> InferTaint.inferSensitiveSink(src)
      2 -> {}
      else -> {}
    }
  }

  fun switchBad2(i: Int) {
    val src = InferTaint.inferSecretSource()
    when (i) {
      1 -> {}
      2 -> InferTaint.inferSensitiveSink(src)
      else -> {}
    }
  }

  fun whileBad1(input: Int) {
    var i = input
    val src = InferTaint.inferSecretSource()
    while (i < 10) {
      InferTaint.inferSensitiveSink(src)
      i++
    }
  }

  fun whileBad2(input: Int) {
    var i = input
    var src: Any? = null
    while (i < 10) {
      src = InferTaint.inferSecretSource()
      i++
    }
    InferTaint.inferSensitiveSink(src)
  }

  // this should report only two alarms, not three
  fun noTripleReportBad() {
    val src = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(src)
    InferTaint.inferSensitiveSink(src)
  }

  fun arrayWithTaintedContentsBad() {
    val src = InferTaint.inferSecretSource()
    val arr = arrayOf(src)
    InferTaint.inferSensitiveSink(arr)
  }

  fun passToSinkOk(x: Int, src: Any?) {
    InferTaint.inferSensitiveSink(src)
  }

  fun funCallBad() {
    val src = InferTaint.inferSecretSource()
    passToSinkOk(2, src)
  }

  /** should not report on these tests */
  fun directOk1() {
    notASink(notASource())
  }

  fun directOk2() {
    notASink(InferTaint.inferSecretSource())
  }

  fun directOk3() {
    InferTaint.inferSensitiveSink(notASource())
  }

  fun viaVarOk() {
    val src = Any()
    InferTaint.inferSensitiveSink(src)
  }

  fun viaVarStrongUpdateOk() {
    var src: Any? = InferTaint.inferSecretSource()
    src = null
    InferTaint.inferSensitiveSink(src)
  }

  fun exceptionOk(b: Boolean, o: Any): Any {
    if (b) {
      throw AssertionError("exception")
    }
    o.toString()
    return o
  }

  fun synchronizedOk(o: Any) {
    synchronized(o) {}
  }

  // this is to test that we don't crash due to the slightly odd translation of synchronized
  fun callSynchronizedOk(o: Any) {
    synchronizedOk(o)
  }

  fun deadCodeOk() {
    val src = InferTaint.inferSecretSource()
    val b = false
    if (b) {
      InferTaint.inferSensitiveSink(src)
    }
  }

  fun loopInvariantOk() {
    var src: Any? = InferTaint.inferSecretSource()
    for (i in 0..9) {
      src = null
    }
    InferTaint.inferSensitiveSink(src)
  }

  fun taintedToSanitizedToSinkOk() {
    InferTaint.inferSensitiveSink(
        InferTaint.inferUniversalSanitizer(InferTaint.inferSecretSource()))
  }

  fun taintOnUnrelatedBooleanOk(notTaintedFlag: Boolean) {
    val taintedFlag = InferTaint.inferSecretSource() as Boolean
    val uberFlag = notTaintedFlag || taintedFlag
    if (!uberFlag) {
      InferTaint.inferSensitiveSink(notTaintedFlag)
    }
  }

  fun javaSourceBad() {
    val src = InferJavaTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(src)
  }

  fun javaSinkBad() {
    val src = InferTaint.inferSecretSource()
    InferJavaTaint.inferSensitiveSink(src)
  }
}
