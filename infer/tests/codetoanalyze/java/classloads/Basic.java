/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class Basic {
  void foo_report(Other o) {
    o.bar();
  }

  void baz_no_report(Another a) {
    a.baz();
  }
}

class Other {
  void bar() {}
}

class Another {
  void baz() {}
}
