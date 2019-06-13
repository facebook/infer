/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Static {
  // this loads StaticA
  static StaticA s = new StaticA();

  public static void main(String args[]) {
    // this loads StaticD
    System.out.println(StaticD.static_data);
  }
}

class StaticA {
  // this loads StaticB
  static StaticB b = new StaticB();
}

class StaticB {
  // no load here
  static StaticCNoLoad c = null;
}

class StaticCNoLoad {}

class StaticD {
  static int static_data = 5;
}
