/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.crashcontext;

public class BranchingCallsExample {

  public static void pre_bar() {
    System.out.println("This runs before the crash.");
  }

  public static void post_bar() {
    System.out.println("This doesn't.");
  }

  public static void bar() {
    String s = null;
    s.toString();
  }

  public static void foo() {
    pre_bar();
    bar();
    post_bar();
  }

  public static void main(String[] args) {
    foo();
  }

}
