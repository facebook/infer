/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.tracing;

public class ReportOnMainExample {

  T2 t;

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
