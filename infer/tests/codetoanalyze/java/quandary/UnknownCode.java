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

/** testing how the analysis handles missing/unknown code */

public class UnknownCode {

  native static Object id(Object o);

  public UnknownCode() {}

  static void propagateViaUnknownCodeBad() {
    Object source = InferTaint.inferSecretSource();
    Object launderedSource = id(source);
    InferTaint.inferSensitiveSink(launderedSource);
  }

  static void propagateViaUnknownConstructorBad() {
    String source = (String) InferTaint.inferSecretSource();
    // we don't analyze the code for the core Java libraries, so this constructor will be unknown
    String unknownConstructor = new String(source);
    InferTaint.inferSensitiveSink(unknownConstructor);
  }

  static void propagateViaUnknownConstructorOk() {
    String unknownConstructor = new String("");
    InferTaint.inferSensitiveSink(unknownConstructor);
  }

  static void propagateViaUnknownCodeOk() {
    Object notASource = new UnknownCode();
    Object launderedSource = id(notASource);
    InferTaint.inferSensitiveSink(launderedSource);
  }

}
