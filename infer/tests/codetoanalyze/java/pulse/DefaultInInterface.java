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

  static void uncertainCallMethod1NPE_latent(int i) {
    A aAorB = new A();
    if (i > 0) { // feasible path
      aAorB = new B();
    }
    System.out.println(aAorB.defaultMethod1().toString());
  }

  static void uncertainCallMethod2NPE(int i) {
    A aAorB = new A();
    if (i > 0) { // feasible path
      aAorB = new B();
    }
    System.out.println(aAorB.defaultMethod2().toString());
  }
}
