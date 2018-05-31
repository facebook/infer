/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
