/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.tracing;

public class UnavoidableExceptionExample {

  static T2 create() {
    return null;
  }

  static void cannotAvoidNPE() {
    T2 t = create();
    t.f();
  }

  static void unavoidableNPEWithParameter(boolean b) {
    T2 t = create();
    t.f();
  }

  void virtualMethodWithUnavoidableNPE(boolean b) {
    T2 t = create();
    t.f();
  }

}
