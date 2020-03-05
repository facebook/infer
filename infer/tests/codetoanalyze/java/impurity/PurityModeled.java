/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;

class PurityModeled {

  double math_random_impure() {
    return Math.random();
  }

  double math_random_infeasible_pure(int x) {
    if (x > 1 && x < 2) {
      return Math.random(); // this path will never be taken
    }
    return 0;
  }

  void arraycopy_pure_FP(int[] src) {
    int[] dst = {5, 10, 20, 30, 40, 50};
    // copies an array from the specified source array
    System.arraycopy(src, 0, dst, 0, 1);
  }

  public void array_length_loop_pure(Integer[] array) {
    for (int i = 0; i < array.length; i++) {}
  }

  void write_impure() {
    byte[] temp = new byte[4];
    System.out.write(temp, 0, 4);
  }

  void call_write_impure() {
    write_impure();
  }

  int math_random_in_loop_impure(int x) {
    int p = 0;
    for (int i = 0; i < x; i++) {
      p += Math.random();
      call_write_impure();
    }

    return p;
  }

  void list_size_pure(ArrayList<String> list) {
    for (int i = 0; i < list.size(); i++) {}
  }

  void list_add_impure(ArrayList<String> list) {
    list.add("a");
  }

  void list_addall_impure(ArrayList<String> list1, ArrayList<String> list2) {
    list1.addAll(list2);
  }

  void enum_loop_pure(Enumeration<String> e) {

    for (; e.hasMoreElements(); ) {
      Object o = e.nextElement();
    }
  }

  void remove_impure(Iterator<String> i) {
    while (i.hasNext()) {
      if (i.next().equals("Orange")) {
        i.remove();
        break;
      }
    }
  }

  void list_set_impure(ArrayList<String> list) {
    list.set(0, "e");
  }

  void timing_call_in_loop_impure_FN() {
    for (int i = 0; i < 10; i++) {
      System.nanoTime();
    }
  }
}
