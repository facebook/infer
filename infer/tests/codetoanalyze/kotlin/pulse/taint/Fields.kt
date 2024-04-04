/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class Fields {

  class Obj {
    var f: Any? = null
    var g: Obj? = null
  }

  var mFld: Any? = null

  companion object {
    var sFld: Any? = null
    var sourceField: Any? = null
    var sinkField: Any? = null
    var regularField: Any? = null
  }

  fun instanceFieldBad() {
    mFld = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(mFld)
  }

  fun staticFieldBad() {
    sFld = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(sFld)
  }

  fun viaFieldBad1(obj: Obj) {
    obj.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.f)
  }

  fun viaFieldBad2() {
    val obj = Obj()
    obj.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.f)
  }

  fun viaFieldBad3() {
    val obj = Obj()
    obj.f = InferTaint.inferSecretSource()
    val src = obj.f
    InferTaint.inferSensitiveSink(src)
  }

  fun viaNestedFieldBad1(obj: Obj) {
    obj.g!!.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.g!!.f)
  }

  fun viaNestedFieldBad2() {
    val obj = Obj()
    obj.g = Obj()
    obj.g!!.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.g!!.f)
  }

  fun viaFieldOk() {
    val obj = Obj()
    obj.f = InferTaint.inferSecretSource()
    obj.g = Obj()
    InferTaint.inferSensitiveSink(obj.g)
  }

  fun viaFieldStrongUpdateOk() {
    val obj = Obj()
    obj.f = InferTaint.inferSecretSource()
    obj.f = null
    InferTaint.inferSensitiveSink(obj.f)
  }

  fun viaNestedFieldOK1(obj: Obj) {
    obj.g!!.f = InferTaint.inferSecretSource()
    obj.g!!.f = null
    InferTaint.inferSensitiveSink(obj.g!!.f)
  }

  fun viaNestedFieldOK2() {
    val obj = Obj()
    obj.g = Obj()
    obj.g!!.f = InferTaint.inferSecretSource()
    obj.g!!.f = null
    InferTaint.inferSensitiveSink(obj.g!!.f)
  }

  fun aliasBad1() {
    val obj1 = Obj()
    obj1.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj1.f)
  }

  fun aliasBad2(obj: Obj) {
    val x = obj.g
    x!!.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(obj.g!!.f)
  }

  fun loopFieldTwoIterationsBad(obj: Obj, i: Int) {
    var i = i
    var loopObj: Obj? = obj
    while (i < 10) {
      loopObj!!.f = InferTaint.inferSecretSource()
      loopObj = loopObj.g
      i++
    }
    InferTaint.inferSensitiveSink(obj.g!!.f)
  }

  fun FN_loopFieldFiveIterationsBad(obj: Obj, i: Int) {
    var i = i
    var loopObj: Obj? = obj
    while (i < 10) {
      loopObj!!.f = InferTaint.inferSecretSource()
      loopObj = loopObj.g
      i++
    }
    InferTaint.inferSensitiveSink(obj.g!!.g!!.g!!.g!!.f)
  }

  fun fieldAsSourceOk() {
    InferTaint.inferSensitiveSink(regularField)
  }

  fun fieldAsSinkOk() {
    val source = InferTaint.inferSecretSource()
    regularField = source
  }

  fun fieldAsSourceBad() {
    InferTaint.inferSensitiveSink(sourceField)
  }

  fun fieldAsSinkBad() {
    val source = InferTaint.inferSecretSource()
    sinkField = source
  }
}
