/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.ArrayList;

class HoistNoIndirectMod {

  int id = 0;

  public int increment() {
    id = calcNext();
    return id;
  }

  public int calcNext() {
    return (id + 1);
  }

  public int calcSame() {
    return id;
  }

  public int increment_dont_hoist_FP(int n) {
    for (int i = 0; i < n; i++) {
      id = calcNext(); // shouldn't be hoisted
    }
    return id;
  }

  public int modify_and_increment_dont_hoist_FP(int n) {
    int p = 0;
    for (int i = 0; i < n; i++) {
      p += calcNext();
      id = i;
    }
    return p;
  }

  // modifies list indirectly via aliasing
  public void set_first_to_zero(ArrayList<Integer> list) {
    ArrayList<Integer> l = list;
    if (l != null) {
      l.set(0, 0);
    }
  }

  public void call_set_first_to_zero(ArrayList<Integer> list) {
    set_first_to_zero(list);
  }

  public void alias_call_set_first_to_zero(ArrayList<Integer> list) {
    ArrayList<Integer> l = list;
    set_first_to_zero(l);
  }

  public void indirect_mod_dont_hoist(Integer[] array, ArrayList<Integer> list) {
    for (Integer element : array) {
      set_first_to_zero(list);
      call_set_first_to_zero(list);
      alias_call_set_first_to_zero(list);
    }
  }

  int avg(ArrayList<Integer> list) {
    int sum = 0;
    for (Integer element : list) {
      sum += element;
    }
    return sum;
  }

  public void no_mod_hoist(Integer[] array, ArrayList<Integer> list) {
    for (Integer element : array) {
      avg(list);
    }
  }
}
