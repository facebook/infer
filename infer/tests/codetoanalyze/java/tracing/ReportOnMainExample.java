// Copyright (c) 2015-Present Facebook. All rights reserved.

package codetoanalyze.java.tracing;

public class ReportOnMainExample {

  T t;

  native boolean test();

  void foo() {
    if (test() && t == null) {
      return;
    }
    t.f();
  }

  public static void main(String[] args) {
    ReportOnMainExample example = new ReportOnMainExample();
    example.foo();
  }

}
