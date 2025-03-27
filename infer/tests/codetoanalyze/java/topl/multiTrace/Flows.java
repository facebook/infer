/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
public class Flows {
  static void test(int x) {
    bar(get(x));
  }

  static Object get(int x) {
    if (x < 0) return foo1();
    else return foo2();
  }

  static Object foo1() {
    return new Object();
  }

  static Object foo2() {
    return new Object();
  }

  static void bar(Object x) {}
}
