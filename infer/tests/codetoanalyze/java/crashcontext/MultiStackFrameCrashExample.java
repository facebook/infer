/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.crashcontext;

public class MultiStackFrameCrashExample {

  public static void bar() {
    String s = null;
    s.toString();
  }

  public static void foo() {
    bar();
  }

  public static void main(String[] args) {
    foo();
  }

}
