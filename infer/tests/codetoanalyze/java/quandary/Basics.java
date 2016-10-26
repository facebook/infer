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

/** testing basic intraprocedural functionality: assignment, ifs, loops, casts */

public class Basics {

  native Object notASource();
  native void notASink(Object o);

  /** should report on these tests */

  void directBad() {
    InferTaint.inferSensitiveSink(InferTaint.inferSecretSource());
  }

  void viaVarBad1() {
    Object src = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(src);
  }

  void viaVarBad2() {
    Object src = InferTaint.inferSecretSource();
    Object alias = src;
    InferTaint.inferSensitiveSink(alias);
  }

  void viaVarBad3() {
    Object src = InferTaint.inferSecretSource();
    Object alias = src;
    src = null;
    InferTaint.inferSensitiveSink(alias);
  }

  void viaCastBad1() {
    InferTaint.inferSensitiveSink((String) InferTaint.inferSecretSource());
  }

  void viaCastBad2() {
    Object src = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink((String) src);
  }

  void ifBad1(boolean b) {
    Object src = null;
    if (b) {
      src = InferTaint.inferSecretSource();
    }
    InferTaint.inferSensitiveSink(src);
  }

  void ifBad2(boolean b) {
    Object src = InferTaint.inferSecretSource();
    if (b) {
      src = null;
    }
    InferTaint.inferSensitiveSink(src);
  }

  void ifBad3(boolean b) {
    Object src;
    if (b) {
      src = new Object();
    } else {
      src = InferTaint.inferSecretSource();
    }
    InferTaint.inferSensitiveSink(src);
  }

  void ifBad4(boolean b1, boolean b2) {
    Object src;
    if (b1) {
      src = new Object();
    } else if (b2) {
      src = InferTaint.inferSecretSource();
    } else {
      src = null;
    }
    InferTaint.inferSensitiveSink(src);
  }

  void ifBad5(boolean b) {
    Object src = InferTaint.inferSecretSource();
    if (b) {
      InferTaint.inferSensitiveSink(src);
    }
  }

  void switchBad1(int i) {
    Object src = InferTaint.inferSecretSource();
    switch (i) {
    case 1:
      InferTaint.inferSensitiveSink(src);
      break;
    case 2:
      break;
    default:
      break;
    }
  }

  void switchBad2(int i) {
    Object src = InferTaint.inferSecretSource();
    switch (i) {
    case 1:
      break;
    case 2:
      InferTaint.inferSensitiveSink(src);
      break;
    default:
      break;
    }
  }

  void switchBad3(int i) {
    Object src = null;
    switch (i) {
    case 1:
      src = InferTaint.inferSecretSource();
      // fallthrough
    case 2:
      InferTaint.inferSensitiveSink(src);
      break;
    default:
      break;
    }
  }

  void whileBad1(int i) {
    Object src = InferTaint.inferSecretSource();
    while (i < 10) {
      InferTaint.inferSensitiveSink(src);
      i++;
    }
  }

  void whileBad2(int i) {
    Object src = null;
    while (i < 10) {
      src = InferTaint.inferSecretSource();
      i++;
    }
    InferTaint.inferSensitiveSink(src);
  }

  // this should report only two alarms, not three
  void noTripleReportBad() {
    Object src = InferTaint.inferSecretSource();
    InferTaint.inferSensitiveSink(src);
    InferTaint.inferSensitiveSink(src);
  }

  void arrayWithTaintedContentsBad() {
    Object src = InferTaint.inferSecretSource();
    Object[] arr = new Object[] { src };
    InferTaint.inferSensitiveSink(arr);
  }

  /** should not report on these tests */

  void directOk1() {
    notASink(notASource());
  }

  void directOk2() {
    notASink(InferTaint.inferSecretSource());
  }

  void directOk3() {
    InferTaint.inferSensitiveSink(notASource());
  }

  void viaVarOk() {
    Object src = new Object();
    InferTaint.inferSensitiveSink(src);
  }

  void viaVarStrongUpdateOk() {
    Object src = InferTaint.inferSecretSource();
    src = null;
    InferTaint.inferSensitiveSink(src);
  }

  Object exceptionOk(boolean b, Object o) {
    if (b) {
      throw new AssertionError("exception");
    }
    o.toString();
    return o;
  }

  /** "known false positive" tests demonstrating limitations. an ideal analysis would not report on
      these tests, but we do. */

  void FP_deadCodeOk() {
    Object src = InferTaint.inferSecretSource();
    boolean b = false;
    if (b) {
      InferTaint.inferSensitiveSink(src);
    }
  }

  void FP_loopInvariantOk() {
    Object src = InferTaint.inferSecretSource();
    for (int i = 0; i < 10; i++) {
      src = null;
    }
    InferTaint.inferSensitiveSink(src);
  }

}
