/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.crashcontext;

public class MethodNameClashExample {

  public static class A {

    public static void foo() {
      String s = null;
      s.toString();
    }

  }

  public static class B {

    public static void foo() {
      A.foo();
    }

  }

  public static void main(String[] args) {
    B.foo();
  }

}
