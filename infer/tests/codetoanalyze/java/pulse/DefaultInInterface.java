/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

public class DefaultInInterface {

  static interface I {
    default Object defaultMethod1() {
      return null;
    }

    default Object defaultMethod2() {
      return "foo";
    }
  }

  public static class A implements I {
    public void defaultCallNPE() {
      System.out.println(this.defaultMethod1().toString());
    }

    public void defaultCallOk() {
      System.out.println(this.defaultMethod2().toString());
    }
  }

  public static class B extends A {
    public Object defaultMethod1() {
      return "foo";
    }

    public Object defaultMethod2() {
      return null;
    }

    public void overridenCallOk() {
      System.out.println(this.defaultMethod1().toString());
    }

    public void overridenCallNPE() {
      System.out.println(this.defaultMethod2().toString());
    }
  }

  static void FN_uncertainCallNPE(int i) {
    A firstAthenB = new A();
    if (i > 0) { // feasible path
      firstAthenB = new B();
    }
    System.out.println(firstAthenB.defaultMethod1().toString());
  }

  static boolean alwaysFalse() {
    return false;
  }

  static void FP_uncertainCallOk(int i) {
    A firstAthenB = new A();
    if (alwaysFalse()) { // unfeasible path
      firstAthenB = new B();
    }
    System.out.println(firstAthenB.defaultMethod2().toString());
  }
}
