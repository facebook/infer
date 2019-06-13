/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Super {
  public static void main(String args[]) {
    // this loads SuperB and SuperA
    System.out.println(SuperB.static_data);

    // this loads SuperC and SuperD
    SuperD.foo();
  }
}

class SuperA {}

class SuperB extends SuperA {
  static int static_data = 5;
}

class SuperC {}

class SuperD extends SuperC {
  public static void foo() {}
}
