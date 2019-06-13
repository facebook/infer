/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Fields {
  FieldsA a;

  Fields() {
    a = new FieldsA();
  }

  void foo() {
    System.out.println(a.b.c);
  }

  public static void main(String args[]) {
    Fields f = new Fields();
    f.foo();
  }
}

class FieldsA {
  FieldsA() {
    b = new FieldsB();
  }

  FieldsB b;
}

class FieldsB {
  FieldsB() {
    c = null;
  }

  FieldsCNoLoad c;
}

class FieldsCNoLoad {}
