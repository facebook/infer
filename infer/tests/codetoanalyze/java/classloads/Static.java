/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Static {
  // this loads StaticA
  static StaticA s = new StaticA();

  public static void main(String args[]) {}
}

class StaticA {
  // this loads StaticB
  static StaticB b = new StaticB();
}

class StaticB {
  // no load here
  static StaticC c = null;
}

class StaticC {}
