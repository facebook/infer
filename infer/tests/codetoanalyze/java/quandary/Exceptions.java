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

class Exceptions {

  native static void mayExcept() throws Exception;

  public static void sinkInCatchBad1() {
    Object source = InferTaint.inferSecretSource();
    try {
      mayExcept();
    } catch (Exception e) {
      InferTaint.inferSensitiveSink(source);
    }
  }

  public static void sinkInCatchBad2() {
    Object source = null;
    try {
      source = InferTaint.inferSecretSource();
      mayExcept();
    } catch (Exception e) {
      InferTaint.inferSensitiveSink(source);
    }
  }

  public static void sinkAfterCatchBad() {
    Object source = InferTaint.inferSecretSource();
    try {
      mayExcept();
      source = null;
    } catch (Exception e) {
    }
    InferTaint.inferSensitiveSink(source);
  }

  public static void sinkAfterCatchOk() {
    Object source = InferTaint.inferSecretSource();
    try {
      mayExcept();
      source = null;
    } catch (Exception e) {
      source = null;
    }
    InferTaint.inferSensitiveSink(source);
  }

  public static void sinkInFinallyBad1() throws Exception{
    Object source = InferTaint.inferSecretSource();
    try {
      mayExcept();
    } finally {
      InferTaint.inferSensitiveSink(source);
    }
  }

  public static void sinkInFinallyBad2() throws Exception {
    Object source = null;
    try {
      mayExcept();
      source = InferTaint.inferSecretSource();
    } finally {
      InferTaint.inferSensitiveSink(source);
    }
  }

  public static void sinkInFinallyBad3() {
    Object source = null;
    try {
      mayExcept();
    } catch (Exception e) {
      source = InferTaint.inferSecretSource();
    } finally {
      InferTaint.inferSensitiveSink(source);
    }
  }

  public static void sinkAfterFinallyOk1() throws Exception {
    Object source = InferTaint.inferSecretSource();
    try {
      mayExcept();
    } finally {
      source = null;
    }
    InferTaint.inferSensitiveSink(source);
  }

  public static void sinkAfterFinallyOk2() {
    Object source = null;
    try {
      mayExcept();
      source = InferTaint.inferSecretSource();
    } catch (Exception e) {
      source = InferTaint.inferSecretSource();
    } finally {
      source = null;
    }
    InferTaint.inferSensitiveSink(source);
  }

  public static void callSinkThenThrow(Object param) throws Exception {
    InferTaint.inferSensitiveSink(param);
    throw new Exception();
  }

  public static void callSinkThenThrowBad() throws Exception {
    callSinkThenThrow(InferTaint.inferSecretSource());
  }

}
