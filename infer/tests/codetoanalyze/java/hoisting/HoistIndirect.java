/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class HoistIndirect {

  class Test {

    int a = 0;

    void set_test(Test test) {
      test.a = 5;
    }

    int get_test(Test test) {
      return test.a;
    }

    int indirect_modification_dont_hoist_FP(int size) {
      int d = 0;
      Test t = new Test();

      for (int i = 0; i < size; i++) {
        set_test(t);
        d = get_test(t); // don't hoist since t changes
      }
      return d;
    }
  }
}
