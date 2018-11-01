/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class HoistIndirect {

  public static int svar = 0;
  int[] array;

  class Test {

    int a = 0;

    int foo(int x) {
      return x + 10;
    }

    void set_test(Test test) {
      test.a = 5;
    }

    int get_test(Test test) {
      return test.a;
    }

    int get_sum_test(Test test, int x) {
      return test.a + x;
    }

    Test return_only(Test t) {
      return t;
    }

    int indirect_modification_dont_hoist(int size, Test t) {
      int d = 0;
      for (int i = 0; i < size; i++) {
        set_test(t);
        d = get_test(t); // don't hoist since t changes
      }
      return d;
    }

    void variant_arg_dont_hoist(int size, Test t) {
      for (int i = 0; i < size; i++) {
        set_test(t); // t is invalidated
        get_sum_test(
            return_only(t),
            size); // foo' and return_only's arguments are variant, hence don't hoist
      }
      ;
    }

    // t changes deep in the call stack
    int deep_modification_dont_hoist(int size) {
      int d = 0;
      Test t = new Test();

      for (int i = 0; i < size; i++) {
        indirect_modification_dont_hoist(size, t);
      }
      return d;
    }

    // foo(3) is ok to hoist, but can't detect this right now
    int indirect_modification_hoist_FN(int size) {
      int d = 0;
      Test t = new Test();
      for (int i = 0; i < size; i++) {
        set_test(t); // this (and t) is invalidated here
        d = foo(3); // foo becomes variant due to implicit arg. this being invalidated above
      }
      return d;
    }
  }

  void set() {
    svar = 5;
  }

  int get() {
    return svar;
  }

  int indirect_this_modification_dont_hoist(int size) {
    int d = 0;

    for (int i = 0; i < size; i++) {
      d = get(); // don't hoist since this.svar changes in the loop
      set();
    }
    return d;
  }

  int direct_this_modification_dont_hoist_FP(int size) {
    int d = 0;

    for (int i = 0; i < size; i++) {
      d += get(); // don't hoist since this.svar changes in the loop
      svar = i;
    }
    return d;
  }

  int this_modification_outside_hoist(int size) {
    int d = 0;
    set();
    for (int i = 0; i < size; i++) {
      d += get(); // ok to hoist since set is outside
    }
    return d;
  }

  int arg_modification_hoist(int size, Test t) {
    int d = 0;
    for (int i = 0; i < size; i++) {
      d += get(); // ok to hoist since set_test doesn't modify this
      t.set_test(t);
    }
    return d;
  }

  void set_ith(int i, int[] array) {
    array[i] = 0;
  }

  int get_ith(int i, int[] array) {
    return array[i];
  }

  int modified_array_dont_hoist(int size, Test t) {
    int d = 0;
    for (int i = 0; i < size; i++) {
      set_ith(i, array);
      d += get_ith(size, array); // don't hoist since array changes
    }
    return d;
  }

  static int regionFirst(int[] region) {
    return region[0];
  }

  static void incrDest(int[] source, int[] dest) {
    dest[0] = source[0] + 1;
  }

  void nested_change_dont_hoist_FP(int[][] nextRegionM, int p, int[] tempRegion) {
    for (int i = 0; i < 10; i++) {
      if (i < regionFirst(nextRegionM[p])) {
        incrDest(tempRegion, nextRegionM[p]);
      }
    }
  }
}
