/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import com.facebook.infer.builtins.InferTaint;

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

  public static void FP_trackParamsOk() {
    InferTaint.inferSensitiveSink(returnSourceConditional(false));
  }

   public static void reassignInCallee(Obj o) {
     o.f = null;
  }

  public static void FP_reassignInCallee() {
    Obj o = new Obj();
    o.f = InferTaint.inferSecretSource();
    reassignInCallee(o);
    InferTaint.inferSensitiveSink(o.f);
  }

  /** false negatives: an ideal analysis would report on these, but we do not */

  // this gets modeled as Object[] params in Java. need to treat the array is tainted if its
  // contents are tainted in order to get this (t13493230).
  public static void callSinkVariadic(Object... params) {
    InferTaint.inferSensitiveSink(params);
  }

  public static void callSinkVariadicBad() {
    callSinkVariadic(null, null, InferTaint.inferSecretSource());
  }

  void diverge() {
    for (;;);
  }

  // we don't propagate divergence in callees to callers
  public void FP_divergenceInCallee() {
    Object source = InferTaint.inferSecretSource();
    diverge();
    InferTaint.inferSensitiveSink(source);
  }

  public static void callSinkThenDiverge(Object param) {
    InferTaint.inferSensitiveSink(param);
    for (;;);
  }

  // this fails because the exit block in callSinkThenDiverge is unreachable, so the summary is
  // empty.
  public void FN_callSinkThenDivergeBad() {
    callSinkThenDiverge(InferTaint.inferSecretSource());
  }

  public static void assignSourceToParam(Object o) {
    o = InferTaint.inferSecretSource();
  }

  // Java is call-by-value; this is fine
  public static void assignSourceToParamOk() {
    Object o = null;
    assignSourceToParam(o);
    InferTaint.inferSensitiveSink(o);
  }

  public static void swapParams(Object o1, Object o2) {
    o1 = o2;
  }

  public static void swapParamsOk() {
    Object notASource = null;
    Object source = InferTaint.inferSecretSource();
    swapParams(notASource, source);
    InferTaint.inferSensitiveSink(notASource);
  }

}
