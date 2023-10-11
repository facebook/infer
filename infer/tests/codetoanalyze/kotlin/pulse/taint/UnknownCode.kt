/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

/** testing how the analysis handles missing/unknown code */
abstract class UnknownCode {

  abstract fun abstractMethod(o: Any?): Any

  interface Interface {
    fun interfaceMethod(o: Any?): Any
  }

  fun propagateViaUnknownCodeOk(i: Interface) {
    val notASource = Any()
    val launderedSource1 = nativeMethod(notASource)
    val launderedSource2 = abstractMethod(launderedSource1)
    val launderedSource3 = i.interfaceMethod(launderedSource2)
    InferTaint.inferSensitiveSink(launderedSource3)
  }

  fun propagateEmptyBad() {
    val source = InferTaint.inferSecretSource() as String
    val buffer = StringBuffer()
    buffer.append(source) // buffer is now tainted
    // even though "" is not tainted, buffer and alias should still be tainted
    val alias = buffer.append("")
    InferTaint.inferSensitiveSink(buffer) // should report
    InferTaint.inferSensitiveSink(alias) // should report
  }

  fun propagateFootprint(param: String?) {
    val buffer = StringBuffer()
    buffer.append(param)
    InferTaint.inferSensitiveSink(buffer)
  }

  fun callPropagateFootprintBad() {
    propagateFootprint(InferTaint.inferSecretSource() as String)
  }

  fun propagateTaint(param: String): String? {
    val i = 1234
    return nativeMethod2(param, i) as String?
  }

  fun callPropagateThenPropagateFootprintBad() {
    val source = InferTaint.inferSecretSource() as String
    propagateFootprint(propagateTaint(source))
  }

  fun propagateViaUnknownNativeCodeBad() {
    val source = InferTaint.inferSecretSource()
    val launderedSource = nativeMethod(source)
    InferTaint.inferSensitiveSink(launderedSource)
  }

  companion object {

    external fun nativeMethod(o: Any?): Any

    fun propagateViaUnknownConstructorBad() {
      val source = InferTaint.inferSecretSource() as String
      // we don't analyze the code for the core Java libraries, so this constructor will be unknown
      val unknownConstructor = java.lang.String(source) as String
      InferTaint.inferSensitiveSink(unknownConstructor)
    }

    fun propagateViaUnknownConstructorOk() {
      val unknownConstructor = ""
      InferTaint.inferSensitiveSink(unknownConstructor)
    }

    external fun nativeMethod2(o: Any?, i: Int): Any?

    fun propagateViaInterfaceCodeBad(i: Interface) {
      val source = InferTaint.inferSecretSource()
      val launderedSource = i.interfaceMethod(source)
      InferTaint.inferSensitiveSink(launderedSource)
    }

    fun propagateViaUnknownAbstractCodeBad() {
      val source = InferTaint.inferSecretSource()
      val launderedSource = nativeMethod(source)
      InferTaint.inferSensitiveSink(launderedSource)
    }
  }
}
