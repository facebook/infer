/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class VirtCall {
  public static void main(String[] args) {
    A a = new A();
    A b = new B();

    A a2 = a.f();
    String x1 = a.g();
    String y1 = a2.g();

    A b2 = b.f();
    String x2 = b.g();
    String y2 = b2.g();
  }
}

class A {
  public A f() {
    return new C();
  }

  public String g() {
    return new String("A");
  }
}

class B extends A {
  @Override
  public String g() {
    return new String("B");
  }
}

class C extends A {
  @Override
  public String g() {
    return new String("C");
  }
}

class D extends A {
  @Override
  public String g() {
    return new String("D");
  }
}
