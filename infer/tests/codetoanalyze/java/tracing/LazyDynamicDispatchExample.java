/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.tracing;

interface I {
  T get();
}

class A implements I {

  public T get() {
    return new T();
  }

}

class B extends A {

  public T get() {
    return null;
  }

}

public class LazyDynamicDispatchExample {

  static T fromSupertype(A a) {
    return a.get();
  }

  static T fromInterface(I i) {
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
