/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class VirtCall {
  public static void main(String[] args) {
    A[] as = {new A(), new B()};
    for (A a : as) {
      A a2 = a.f();
      String x = a.g();
      String y = a2.g();
      System.out.println(x);
      System.out.println(y);
    }
  }
}

class A {
  public A f() {
    return new C();
  }

  public String g() {
    return "A";
  }
}

class B extends A {
  @Override
  public String g() {
    return "B";
  }
}

class C extends A {
  @Override
  public String g() {
    return "A";
  }
}

class D extends A {
  @Override
  public String g() {
    return "D";
  }
}
