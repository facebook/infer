/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.tracing;

interface I {
  T2 get();
}

class A implements I {

  public T2 get() {
    return new T2();
  }
}

class B extends A {

  public T2 get() {
    return null;
  }
}

public class LazyDynamicDispatchExample {

  static T2 fromSupertype(A a) {
    return a.get();
  }

  static T2 fromInterface(I i) {
    return i.get();
  }

  static void callWithSubtype() {
    B b = new B();
    fromSupertype(b).f();
  }

  static void shouldNotReportLocalVarTypeIsKnown() {
    A a = new A();
    fromInterface(a).f();
  }

  static void shouldReportLocalVarTypeIsKnown() {
    B b = new B();
    fromInterface(b).f();
  }
}
