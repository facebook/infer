/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class Arrays {

  class Obj {
    var f: Any? = null
    var arr: Array<Any?>? = null
  }

  /** should report on these tests */
  fun viaArrayBad() {
    val arr = arrayOfNulls<Any>(1)
    arr[0] = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(arr[0])
  }

  fun viaArrayThenFieldBad() {
    val arr = arrayOfNulls<Obj>(1)
    arr[0]!!.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(arr[0]!!.f)
  }

  fun viaFieldThenArrayBad1(obj: Obj) {
    obj.arr!![0] = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.arr!![0])
  }

  fun viaFieldThenArrayBad2() {
    val obj = Obj()
    obj.arr = arrayOfNulls(1)
    obj.arr!![0] = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.arr!![0])
  }

  /** should not report on these tests */
  fun viaArrayOk() {
    val arr = arrayOfNulls<Any>(1)
    arr[0] = Any()
    InferTaint.inferSensitiveSink(arr[0])
  }

  fun viaArrayOk1(y: Any?, z: Array<Any?>?) {
    val arr = arrayOfNulls<Any>(2)
    arr[0] = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(arr[1])
  }

  fun viaArrayOk2(y: Any?, z: Array<Any?>?) {
    val arr = arrayOfNulls<Any>(1)
    arr[0] = InferTaint.inferSecretSource()
    arr[0] = null
    InferTaint.inferSensitiveSink(arr[0])
  }
}
