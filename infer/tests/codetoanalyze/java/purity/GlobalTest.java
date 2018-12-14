/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
class GlobalTest {
  public static int s = 0;

  class Foo {

    // modifies global var 's' hence impure
    void set_bad() {
      s = 10;
    }
  }

  // calls foo which modifies global var
  void call_set_bad() {
    Foo f = new Foo();
    f.set_bad();
  }
}
