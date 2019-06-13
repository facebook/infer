/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

class HoistIndirect {

  public static int svar = 0;
  public int x = 0;
  int[] array;

  class Test {

    int a = 0;
    int[] test_array;

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
        get_sum_test(return_only(t), size); // return_only's argument is variant, hence don't hoist
      }
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

    // foo(3) is ok to hoist
    int indirect_modification_hoist(int size) {
      int d = 0;
      Test t = new Test();
      for (int i = 0; i < size; i++) {
        set_test(t); // t is invalidated here
        d = foo(3); // foo is still invariant so it is ok to hoist
      }
      return d;
    }

    void set_only_first_param(Test test, Test no_mod) {
      test.a = 5;
    }

    int indirect_modification_only_second_call_hoist(int size, Test t, Test no_mod_t) {
      int d = 0;
      for (int i = 0; i < size; i++) {
        set_only_first_param(t, no_mod_t);
        d = get_test(t); // don't hoist since t changes
        d = get_test(no_mod_t); // hoist since no_mod_t doesn't change
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

  //
  int indirect_this_modification_dont_hoist(int size) {
    int d = 0;

    for (int i = 0; i < size; i++) {
      d = get(); // don't hoist since HoistIndirect.svar changes in the loop
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

  int independent_hoist(int size, Test t) {
    int d = 0;
    for (int i = 0; i < size; i++) {
      set_ith(i, array);
      t.foo(size); // hoist call to foo
      d += get_ith(size, array); // don't hoist since array changes
    }
    return d;
  }

  int modified_inner_array_dont_hoist(int size, Test t) {
    int d = 0;
    for (int i = 0; i < size; i++) {
      set_ith(i, t.test_array);
      d += t.foo(t.test_array[0]); // don't hoist since t.test_array changes
    }
    return d;
  }

  static int regionFirst(int[] region) {
    return region[0];
  }

  static void incrDest(int[] source, int[] dest) {
    dest[0] = source[0] + 1;
  }

  static void sumDest(int[] source, int[] dest, int x) {
    dest[0] = source[0] + x;
  }

  void irvar_change_dont_hoist(int[][] nextRegionM, int p, int[] tempRegion) {
    for (int i = 0; i < 10; i++) {
      if (i < regionFirst(nextRegionM[p])) {
        incrDest(tempRegion, nextRegionM[p]); // invalidate nextRegionM
      }
    }
  }

  void tmp_irvar_change_dont_hoist(int[][] nextRegionM, int p, int[] tempRegion) {
    for (int i = 0; i < 10; i++) {
      if (i < regionFirst(nextRegionM[p])) {
        int[] arr = nextRegionM[p];
        incrDest(tempRegion, arr); // invalidate nextRegionM
      }
    }
  }

  int double_me(int p) {
    return 2 * p;
  }

  void irvar_independent_hoist(int[][] nextRegionM, int p, int[] tempRegion) {
    for (int i = 0; i < 10; i++) {
      if (i < regionFirst(nextRegionM[p]) + double_me(p)) { // double_me(p) can be hoisted
        sumDest(tempRegion, nextRegionM[p], i); // invalidate nextRegionM
      }
    }
  }

  void unmodified_arg_hoist(int[][] nextRegionM, int p, int[] tempRegion) {
    for (int i = 0; i < 10; i++) {
      if (i
          < regionFirst(nextRegionM[p])
              + regionFirst(tempRegion)) { // regionFirst(tempRegion) can be hoisted
        sumDest(tempRegion, nextRegionM[p], i); // invalidate nextRegionM
      }
    }
  }

  // arr is modified via aliasing
  void alias(int[] arr) {

    int[] new_arr = arr;
    new_arr[0] = 4;
  }

  void alias_dont_hoist(int[] arr) {
    for (int i = 0; i < 10; i++) {
      alias(arr); // alias modifies arr
      get_ith(0, arr); // don't hoist
    }
  }

  int return_zero() {
    return 0;
  }

  void set_x() {
    x = 0;
  }

  // Since we don't keep track of what values are read in purity
  // analysis, when we call set_x, we add implicit arg. "this" to
  // modified arguments which prevents any other call in the loop to
  // be hoisted
  int indirect_this_modification_hoist_FN(int size) {
    int d = 0;

    for (int i = 0; i < size; i++) {
      d = return_zero(); // OK to hoist
      set_x();
    }
    return d;
  }
}
