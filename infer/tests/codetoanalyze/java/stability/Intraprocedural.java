/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.ThreadSafe;

class Intraprocedural {

static class B {
  int g = 0;
}

static class A {
  B f = new B();
  int h = 0;
}

@ThreadSafe
static class Field {
  private A a = new A();

  public void unstable_ok() {
     int x = 42;
     B b = a.f; // destabilizes
     synchronized (this) {
       a.f.g = 101;
     }
     x = a.f.g;
  }

  public void stable_bad() {
     int x = 42;
     synchronized (this) {
       a.f.g = 101;
     }
     x = a.f.g;
  }
}

static class Param {

  public void unstable_ok(A a) {
    int x = 42;
    B b = a.f; // destabilizes
    synchronized (this) {
      a.f.g = 101;
    }
    x = a.f.g;
  }

  public void stable_bad(A a) {
    int x = 42;
    synchronized (this) {
      a.f.g = 101;
    }
    x = a.f.g;
  }

}

@ThreadSafe
static class Global {
  private static A a = new A();

  synchronized public A getA() {
    return a;
  }

  synchronized public void setA(A newA) {
    a = newA;
  }

  public void unstable_ok() {
     int x = 42;
     A a = getA(); // destabilizes
     synchronized (this) {
       a.f.g = 101;
     }
     x = a.f.g;
  }

}

}
