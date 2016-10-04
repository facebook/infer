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

/** testing how the analysis handles strings and string manipulation functions */

public class Strings {

  static class Wrapper {
    Object f;
  }

  static void valueOfStringBad() {
    Object source = InferTaint.inferSecretSource();
    String stringSource = String.valueOf(source);
    InferTaint.inferSensitiveSink(stringSource);
  }

  static void valueOfStringWrapperBad() {
    Wrapper w = new Wrapper();
    w.f = InferTaint.inferSecretSource();
    String stringSource = String.valueOf(w.f);
    InferTaint.inferSensitiveSink(stringSource);
  }

}
