/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.pulse;

class Interprocedural {

  Object f;

  static Object sGlobal;

  static class Obj {
    Object f;
  }

  public static Object id(Object param) {
    return param;
  }

  /** source tests */
  public static Object returnSourceDirect() {
    return InferTaint.inferSecretSource();
  }

  public static Object returnSourceIndirect() {
    return returnSourceDirect();
  }

  public static void returnSourceDirectBad() {
    InferTaint.inferSensitiveSink(returnSourceDirect());
  }

  public static void returnSourceDirectViaVarBad() {
    Object source = returnSourceDirect();
    InferTaint.inferSensitiveSink(source);
  }

  public static void returnSourceIndirectBad() {
    InferTaint.inferSensitiveSink(returnSourceIndirect());
  }

  public static Obj returnSourceViaField() {
    Obj o = new Obj();
    o.f = InferTaint.inferSecretSource();
    return o;
  }

  public static void returnSourceViaFieldBad() {
    InferTaint.inferSensitiveSink(returnSourceViaField().f);
  }

  public static void returnSourceViaParameter1(Obj o) {
    o.f = InferTaint.inferSecretSource();
  }

  public static void returnSourceViaParameter1Bad(Obj o) {
    returnSourceViaParameter1(o);
    InferTaint.inferSensitiveSink(o.f);
  }

  public static void returnSourceViaParameter2(Obj o1, Obj o2) {
    o2.f = o1.f;
  }

  public static void returnSourceViaParameter2Bad(Obj o1, Obj o2) {
    o1.f = InferTaint.inferSecretSource();
    returnSourceViaParameter2(o1, o2);
    InferTaint.inferSensitiveSink(o2.f);
  }

  public static void returnSourceViaParameterOk(Obj o1, Obj o2) {
    o1.f = InferTaint.inferSecretSource();
    returnSourceViaParameter2(o2, o1);
    InferTaint.inferSensitiveSink(o2.f);
  }

  public static void returnSourceViaGlobal() {
    sGlobal = InferTaint.inferSecretSource();
  }

  public void returnSourceViaGlobalBad() {
    returnSourceViaGlobal();
    InferTaint.inferSensitiveSink(sGlobal);
  }

  public void returnSourceViaGlobalOk() {
    returnSourceViaGlobal();
    sGlobal = null;
    InferTaint.inferSensitiveSink(sGlobal);
  }

  /** sink tests */
  public static void callSinkParam1(Object param1, Object param2) {
    InferTaint.inferSensitiveSink(param1);
  }

  public static void callSinkParam1Bad() {
    callSinkParam1(InferTaint.inferSecretSource(), null);
  }

  public static void callSinkParam1Ok() {
    callSinkParam1(null, InferTaint.inferSecretSource());
  }

  public static void callSinkParam2(Object param1, Object param2) {
    InferTaint.inferSensitiveSink(param2);
  }

  public static void callSinkParam2Bad() {
    callSinkParam2(null, InferTaint.inferSecretSource());
  }

  public static void callSinkParam2Ok() {
    callSinkParam2(InferTaint.inferSecretSource(), null);
  }

  public void callSinkOnFieldDirect() {
    InferTaint.inferSensitiveSink(this.f);
  }

  public void callSinkOnFieldDirectBad() {
    this.f = InferTaint.inferSecretSource();
    callSinkOnFieldDirect();
  }

  public static void callSinkOnFieldIndirect(Obj param) {
    InferTaint.inferSensitiveSink(param.f);
  }

  public static void callSinkOnFieldIndirectBad() {
    Obj obj = new Obj();
    obj.f = InferTaint.inferSecretSource();
    callSinkOnFieldIndirect(obj);
  }

  public Object getF() {
    return f;
  }

  void callSinkOnLocal() {
    Object local = this.getF();
    InferTaint.inferSensitiveSink(local);
  }

  void callSinkOnLocalBad() {
    this.f = InferTaint.inferSecretSource();
    callSinkOnLocal();
  }

  public static void callSinkOnGlobal() {
    InferTaint.inferSensitiveSink(sGlobal);
  }

  public static void callSinkOnGlobalBad() {
    sGlobal = InferTaint.inferSecretSource();
    callSinkOnGlobal();
  }

  public static void callSinkOnGlobalOk() {
    sGlobal = InferTaint.inferSecretSource();
    sGlobal = null;
    callSinkOnGlobal();
  }

  public static void setGlobal(Object o) {
    sGlobal = o;
  }

  public static void setGlobalThenCallSinkBad() {
    setGlobal(InferTaint.inferSecretSource());
    callSinkOnGlobal();
  }

  public static Object getGlobal() {
    return sGlobal;
  }

  public static void getGlobalThenCallSink() {
    Object local = getGlobal();
    InferTaint.inferSensitiveSink(sGlobal);
  }

  public static void getGlobalThenCallSinkBad() {
    sGlobal = InferTaint.inferSecretSource();
    getGlobalThenCallSink();
  }

  // this should report two alarms, not three
  public void callSinkNoTripleReportBad() {
    Object source = InferTaint.inferSecretSource();
    callSinkParam1(source, null);
    callSinkParam2(null, source);
  }

  /** passthrough tests */
  public static void singlePassthroughBad() {
    Object source = InferTaint.inferSecretSource();
    Object launderedSource = id(source);
    InferTaint.inferSensitiveSink(launderedSource);
  }

  public static void doublePassthroughBad() {
    Object source = InferTaint.inferSecretSource();
    Object launderedSource1 = id(source);
    Object launderedSource2 = id(launderedSource1);
    InferTaint.inferSensitiveSink(launderedSource2);
  }

  /** false positives: an ideal analysis would not report these, but we will */
  public static Object returnSourceConditional(boolean b) {
    if (b) return InferTaint.inferSecretSource();
    return null;
  }

  public static void trackParamsOk() {
    InferTaint.inferSensitiveSink(returnSourceConditional(false));
  }

  public static void reassignInCallee(Obj o) {
    o.f = null;
  }

  public static void reassignInCallee() {
    Obj o = new Obj();
    o.f = InferTaint.inferSecretSource();
    reassignInCallee(o);
    InferTaint.inferSensitiveSink(o.f);
  }

  static Object relevantPassthrough(Object param) {
    return param;
  }

  static Object irrelevantPassthrough(Object param) {
    return param;
  }

  // the following tests should show only "relevantPassthrough" in their traces
  public static Object irrelevantPassthroughsIntraprocedural(Object param) {
    Object irrelevant = irrelevantPassthrough(param);
    Object source = InferTaint.inferSecretSource();
    Object relevant = relevantPassthrough(source);
    InferTaint.inferSensitiveSink(relevant);
    return irrelevantPassthrough(relevant);
  }

  public static Object returnSourceIrrelevantPassthrough(Object param) {
    Object irrelevant = irrelevantPassthrough(param);
    Object source = InferTaint.inferSecretSource();
    return relevantPassthrough(source);
  }

  public static Object irrelevantPassthroughsSourceInterprocedural(Object o) {
    Object irrelevant = irrelevantPassthrough(o);
    Object source = returnSourceIrrelevantPassthrough(irrelevant);
    Object relevant = relevantPassthrough(source);
    InferTaint.inferSensitiveSink(relevant);
    return irrelevantPassthrough(source);
  }

  public static Object callSinkIrrelevantPassthrough(Object param) {
    Object relevant = relevantPassthrough(param);
    InferTaint.inferSensitiveSink(relevant);
    Object irrelevant = irrelevantPassthrough(param);
    return irrelevant;
  }

  public static Object irrelevantPassthroughsSinkInterprocedural(Object o) {
    Object source = InferTaint.inferSecretSource();
    Object relevant = relevantPassthrough(source);
    callSinkIrrelevantPassthrough(relevant);
    return irrelevantPassthrough(relevant);
  }

  public static Object irrelevantPassthroughsSourceAndSinkInterprocedural(Object o) {
    Object irrelevant = irrelevantPassthrough(o);
    Object source = returnSourceIrrelevantPassthrough(irrelevant);
    Object relevant = relevantPassthrough(source);
    callSinkIrrelevantPassthrough(relevant);
    return irrelevantPassthrough(relevant);
  }

  public static void callSinkVariadic(Object... params) {
    InferTaint.inferSensitiveSink(params);
  }

  public static void callSinkVariadicBad() {
    callSinkVariadic(null, null, InferTaint.inferSecretSource());
  }

  void diverge() {
    for (; ; ) ;
  }

  public void divergenceInCallee() {
    Object source = InferTaint.inferSecretSource();
    diverge();
    InferTaint.inferSensitiveSink(source);
  }

  public static void callSinkThenDiverge(Object param) {
    InferTaint.inferSensitiveSink(param);
    for (; ; ) ;
  }

  public void FN_callSinkThenDivergeBad() {
    callSinkThenDiverge(InferTaint.inferSecretSource());
  }

  public void callSinkOnParam(Object o) {
    InferTaint.inferSensitiveSink(o);
  }

  public void callSinkIndirectOnParam(Object o) {
    callSinkOnParam(o);
  }

  Obj propagate(Object param) {
    Obj o = new Obj();
    o.f = param;
    return o;
  }

  static Obj id2(Obj o) {
    return o;
  }

  void callSinkA(Obj o) {
    callSink1(o);
  }

  void callSinkB(Obj o) {
    callSink2(o);
  }

  void callSinkC(Obj o) {
    callSink3(o);
  }

  void callSinkD(Obj o) {
    callSink4(o);
  }

  void callSink1(Obj o) {
    InferTaint.inferSensitiveSink(id(o));
  }

  void callSink2(Obj o) {
    InferTaint.inferSensitiveSink(id2(o).f);
  }

  void callSink3(Obj o) {
    InferTaint.inferSensitiveSink(id(o.f));
  }

  void callSink4(Obj o) {
    InferTaint.inferSensitiveSink(o.f);
  }

  public void callDeepSinkIndirectBad() {
    Object source = InferTaint.inferSecretSource();
    callSinkIndirectOnParam(source);
  }

  // the sink is on source but source.f is tainted, not source
  public void FN_callDeepSink1Bad() {
    Obj source = propagate(InferTaint.inferSecretSource());
    callSinkA(source);
  }

  public void callDeepSink2Bad() {
    Obj source = propagate(InferTaint.inferSecretSource());
    callSinkB(source);
  }

  void callShallowSinkBad(Obj o) {
    o.f = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(id2(o).f);
  }

  public void callDeepSink3Bad() {
    Obj source = propagate(InferTaint.inferSecretSource());
    callSinkC(source);
  }

  public void callDeepSink4Bad() {
    Obj source = propagate(InferTaint.inferSecretSource());
    callSinkD(source);
  }

  public static void swapParams(Object o1, Object o2) {
    o1 = o2;
  }

  public static void assignSourceToParam(Object o) {
    o = InferTaint.inferSecretSource();
  }

  public static void swapParamsOk() {
    Object notASource = null;
    Object source = InferTaint.inferSecretSource();
    swapParams(notASource, source);
    InferTaint.inferSensitiveSink(notASource);
  }

  public static void assignSourceToParamOk() {
    Object o = null;
    assignSourceToParam(o);
    InferTaint.inferSensitiveSink(o);
  }
}
