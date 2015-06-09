// Copyright (c) 2015-Present Facebook. All rights reserved.

package codetoanalyze.java.tracing;

public class UnavoidableExceptionExample {

  static T create() {
    return null;
  }

  static void cannotAvoidNPE() {
    T t = create();
    t.f();
  }

  static void unavoidableNPEWithParameter(boolean b) {
    T t = create();
    t.f();
  }

  void virtualMethodWithUnavoidableNPE(boolean b) {
    T t = create();
    t.f();
  }

}
