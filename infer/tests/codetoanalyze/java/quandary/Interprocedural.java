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

  /** false positives: an ideal analysis would not report these, but we will */

  public static Object returnSourceConditional(boolean b) {
    if (b) return InferTaint.inferSecretSource();
    return null;
  }

  public static void FP_trackParamsOk() {
    InferTaint.inferSensitiveSink(returnSourceConditional(false));
  }

}
