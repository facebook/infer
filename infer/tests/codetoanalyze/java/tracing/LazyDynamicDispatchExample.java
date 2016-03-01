/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.tracing;

class A {

  public T get() {
    return new T();
  }

}

class B extends A  {

  public T get() {
    return null;
  }

}

public class LazyDynamicDispatchExample {

  static T foo(A a) {
    return a.get();
  }

  static void bar() {
    B b = new B();
    foo(b).f();
  }

}
