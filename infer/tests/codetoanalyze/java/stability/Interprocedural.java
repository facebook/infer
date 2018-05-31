/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

package codetoanalyze.java.checkers;

import com.facebook.infer.annotation.ThreadSafe;

class Interprocedural {

static class A {
  B f = new B();
  int h = 0;
}

static class B {
  int g = 0;
}

@ThreadSafe
static class Field {
  private A a = new A();

  private void destabilize() {
    B b = a.f;
  }

  public void unstable_ok() {
    int x = 42;
    destabilize();
    synchronized (this){
      a.f.g = 101;
    }
    x = a.f.g;
  }

  public void stable_bad() {
    int x = 42;
    synchronized (this){
      a.f.g = 101;
    }
    x = a.f.g;
  }

}

@ThreadSafe
static class Param {

  private void destabilize(A z) {
    B b1 = z.f;
    System.out.println(b1);
  }

  public void unstable_ok(A a) {
    int x = 42;
    destabilize(a);
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
static class Param2 {

  private void destabilize(A z)   {
    // Do nothing
  }

  public void stable_bad(A a) {
    int x = 42;
    destabilize(a); // a leaks, but shouldn't be de-stabilized because callee does nothing
    synchronized (this) {
      a.f.g = 101;
    }
    x = a.f.g;
  }
}

}
