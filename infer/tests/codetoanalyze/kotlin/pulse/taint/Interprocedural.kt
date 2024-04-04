/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
package codetoanalyze.kotlin.pulse

class Interprocedural {

  var f: Any? = null

  class Obj {
    var f: Any? = null
  }

  fun returnSourceViaGlobalBad() {
    returnSourceViaGlobal()
    InferTaint.inferSensitiveSink(global)
  }

  fun returnSourceViaGlobalOk() {
    returnSourceViaGlobal()
    global = null
    InferTaint.inferSensitiveSink(global)
  }

  fun callSinkOnFieldDirect() {
    InferTaint.inferSensitiveSink(f)
  }

  fun callSinkOnFieldDirectBad() {
    f = InferTaint.inferSecretSource()
    callSinkOnFieldDirect()
  }

  fun callSinkOnLocal() {
    val local = f
    InferTaint.inferSensitiveSink(local)
  }

  fun callSinkOnLocalBad() {
    f = InferTaint.inferSecretSource()
    callSinkOnLocal()
  }

  // this should report two alarms, not three
  fun callSinkNoTripleReportBad() {
    val source = InferTaint.inferSecretSource()
    callSinkParam1(source, null)
    callSinkParam2(null, source)
  }

  fun diverge() {
    while (true) {}
  }

  fun divergenceInCallee() {
    val source = InferTaint.inferSecretSource()
    diverge()
    InferTaint.inferSensitiveSink(source)
  }

  fun FN_callSinkThenDivergeBad() {
    callSinkThenDiverge(InferTaint.inferSecretSource())
  }

  fun callSinkOnParam(o: Any?) {
    InferTaint.inferSensitiveSink(o)
  }

  fun callSinkIndirectOnParam(o: Any?) {
    callSinkOnParam(o)
  }

  fun propagate(param: Any?): Obj {
    val o = Obj()
    o.f = param
    return o
  }

  fun callSinkA(o: Obj?) {
    callSink1(o)
  }

  fun callSinkB(o: Obj) {
    callSink2(o)
  }

  fun callSinkC(o: Obj) {
    callSink3(o)
  }

  fun callSinkD(o: Obj) {
    callSink4(o)
  }

  fun callSink1(o: Obj?) {
    InferTaint.inferSensitiveSink(id(o))
  }

  fun callSink2(o: Obj) {
    InferTaint.inferSensitiveSink(id2(o).f)
  }

  fun callSink3(o: Obj) {
    InferTaint.inferSensitiveSink(id(o.f))
  }

  fun callSink4(o: Obj) {
    InferTaint.inferSensitiveSink(o.f)
  }

  fun callDeepSinkIndirectBad() {
    val source = InferTaint.inferSecretSource()
    callSinkIndirectOnParam(source)
  }

  // the sink is on source but source.f is tainted, not source
  fun FN_callDeepSink1Bad() {
    val source = propagate(InferTaint.inferSecretSource())
    callSinkA(source)
  }

  fun callDeepSink2Bad() {
    val source = propagate(InferTaint.inferSecretSource())
    callSinkB(source)
  }

  fun callShallowSinkBad(o: Obj) {
    o.f = InferTaint.inferSecretSource()
    InferTaint.inferSensitiveSink(id2(o).f)
  }

  fun callDeepSink3Bad() {
    val source = propagate(InferTaint.inferSecretSource())
    callSinkC(source)
  }

  fun callDeepSink4Bad() {
    val source = propagate(InferTaint.inferSecretSource())
    callSinkD(source)
  }

  companion object {
    var global: Any? = null

    fun id(param: Any?): Any? {
      return param
    }

    /** source tests */
    fun returnSourceDirect(): Any {
      return InferTaint.inferSecretSource()
    }

    fun returnSourceIndirect(): Any {
      return returnSourceDirect()
    }

    fun returnSourceDirectBad() {
      InferTaint.inferSensitiveSink(returnSourceDirect())
    }

    fun returnSourceDirectViaVarBad() {
      val source = returnSourceDirect()
      InferTaint.inferSensitiveSink(source)
    }

    fun returnSourceIndirectBad() {
      InferTaint.inferSensitiveSink(returnSourceIndirect())
    }

    fun returnSourceViaField(): Obj {
      val o = Obj()
      o.f = InferTaint.inferSecretSource()
      return o
    }

    fun returnSourceViaFieldBad() {
      InferTaint.inferSensitiveSink(returnSourceViaField().f)
    }

    fun returnSourceViaParameter1(o: Obj) {
      o.f = InferTaint.inferSecretSource()
    }

    fun returnSourceViaParameter1Bad(o: Obj) {
      returnSourceViaParameter1(o)
      InferTaint.inferSensitiveSink(o.f)
    }

    fun returnSourceViaParameter2(o1: Obj, o2: Obj) {
      o2.f = o1.f
    }

    fun returnSourceViaParameter2Bad(o1: Obj, o2: Obj) {
      o1.f = InferTaint.inferSecretSource()
      returnSourceViaParameter2(o1, o2)
      InferTaint.inferSensitiveSink(o2.f)
    }

    fun returnSourceViaParameterOk(o1: Obj, o2: Obj) {
      o1.f = InferTaint.inferSecretSource()
      returnSourceViaParameter2(o2, o1)
      InferTaint.inferSensitiveSink(o2.f)
    }

    fun returnSourceViaGlobal() {
      global = InferTaint.inferSecretSource()
    }

    /** sink tests */
    fun callSinkParam1(param1: Any?, param2: Any?) {
      InferTaint.inferSensitiveSink(param1)
    }

    fun callSinkParam1Bad() {
      callSinkParam1(InferTaint.inferSecretSource(), null)
    }

    fun callSinkParam1Ok() {
      callSinkParam1(null, InferTaint.inferSecretSource())
    }

    fun callSinkParam2(param1: Any?, param2: Any?) {
      InferTaint.inferSensitiveSink(param2)
    }

    fun callSinkParam2Bad() {
      callSinkParam2(null, InferTaint.inferSecretSource())
    }

    fun callSinkParam2Ok() {
      callSinkParam2(InferTaint.inferSecretSource(), null)
    }

    fun callSinkOnFieldIndirect(param: Obj) {
      InferTaint.inferSensitiveSink(param.f)
    }

    fun callSinkOnFieldIndirectBad() {
      val obj = Obj()
      obj.f = InferTaint.inferSecretSource()
      callSinkOnFieldIndirect(obj)
    }

    fun callSinkOnGlobal() {
      InferTaint.inferSensitiveSink(global)
    }

    fun callSinkOnGlobalBad() {
      global = InferTaint.inferSecretSource()
      callSinkOnGlobal()
    }

    fun callSinkOnGlobalOk() {
      global = InferTaint.inferSecretSource()
      global = null
      callSinkOnGlobal()
    }

    fun setGlobalThenCallSinkBad() {
      global = InferTaint.inferSecretSource()
      callSinkOnGlobal()
    }

    val globalThenCallSink: Unit
      get() {
        val local = global
        InferTaint.inferSensitiveSink(global)
      }

    val globalThenCallSinkBad: Unit
      get() {
        global = InferTaint.inferSecretSource()
        globalThenCallSink
      }

    /** passthrough tests */
    fun singlePassthroughBad() {
      val source = InferTaint.inferSecretSource()
      val launderedSource = id(source)
      InferTaint.inferSensitiveSink(launderedSource)
    }

    fun doublePassthroughBad() {
      val source = InferTaint.inferSecretSource()
      val launderedSource1 = id(source)
      val launderedSource2 = id(launderedSource1)
      InferTaint.inferSensitiveSink(launderedSource2)
    }

    /** false positives: an ideal analysis would not report these, but we will */
    fun returnSourceConditional(b: Boolean): Any? {
      return if (b) InferTaint.inferSecretSource() else null
    }

    fun trackParamsOk() {
      InferTaint.inferSensitiveSink(returnSourceConditional(false))
    }

    fun reassignInCallee(o: Obj) {
      o.f = null
    }

    fun reassignInCallee() {
      val o = Obj()
      o.f = InferTaint.inferSecretSource()
      reassignInCallee(o)
      InferTaint.inferSensitiveSink(o.f)
    }

    fun relevantPassthrough(param: Any): Any {
      return param
    }

    fun irrelevantPassthrough(param: Any): Any {
      return param
    }

    // the following tests should show only "relevantPassthrough" in their traces
    fun irrelevantPassthroughsIntraprocedural(param: Any): Any {
      val irrelevant = irrelevantPassthrough(param)
      val source = InferTaint.inferSecretSource()
      val relevant = relevantPassthrough(source)
      InferTaint.inferSensitiveSink(relevant)
      return irrelevantPassthrough(relevant)
    }

    fun returnSourceIrrelevantPassthrough(param: Any): Any {
      val irrelevant = irrelevantPassthrough(param)
      val source = InferTaint.inferSecretSource()
      return relevantPassthrough(source)
    }

    fun irrelevantPassthroughsSourceInterprocedural(o: Any): Any {
      val irrelevant = irrelevantPassthrough(o)
      val source = returnSourceIrrelevantPassthrough(irrelevant)
      val relevant = relevantPassthrough(source)
      InferTaint.inferSensitiveSink(relevant)
      return irrelevantPassthrough(source)
    }

    fun callSinkIrrelevantPassthrough(param: Any): Any {
      val relevant = relevantPassthrough(param)
      InferTaint.inferSensitiveSink(relevant)
      return irrelevantPassthrough(param)
    }

    fun irrelevantPassthroughsSinkInterprocedural(o: Any?): Any {
      val source = InferTaint.inferSecretSource()
      val relevant = relevantPassthrough(source)
      callSinkIrrelevantPassthrough(relevant)
      return irrelevantPassthrough(relevant)
    }

    fun irrelevantPassthroughsSourceAndSinkInterprocedural(o: Any): Any {
      val irrelevant = irrelevantPassthrough(o)
      val source = returnSourceIrrelevantPassthrough(irrelevant)
      val relevant = relevantPassthrough(source)
      callSinkIrrelevantPassthrough(relevant)
      return irrelevantPassthrough(relevant)
    }

    fun callSinkVariadic(vararg params: Any?) {
      InferTaint.inferSensitiveSink(params)
    }

    fun callSinkVariadicBad() {
      callSinkVariadic(null, null, InferTaint.inferSecretSource())
    }

    fun callSinkThenDiverge(param: Any?) {
      InferTaint.inferSensitiveSink(param)
      while (true) {}
    }

    fun id2(o: Obj): Obj {
      return o
    }

    fun swapParams(o1: Any?, o2: Any?) {
      var o1 = o1
      o1 = o2
    }

    fun assignSourceToParam(o: Any?) {
      var o = o
      o = InferTaint.inferSecretSource()
    }

    fun swapParamsOk() {
      val notASource: Any? = null
      val source = InferTaint.inferSecretSource()
      swapParams(notASource, source)
      InferTaint.inferSensitiveSink(notASource)
    }

    fun assignSourceToParamOk() {
      val o: Any? = null
      assignSourceToParam(o)
      InferTaint.inferSensitiveSink(o)
    }
  }
}
