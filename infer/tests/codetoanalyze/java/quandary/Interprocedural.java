/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.quandary;

import com.facebook.infer.models.InferTaint;

class Interprocedural {

  Object f;

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
    return InferTaint.inferSecretSource();
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

  public static void FN_callSinkVariadicBad() {
    callSinkVariadic(null, null, InferTaint.inferSecretSource());
  }

}
