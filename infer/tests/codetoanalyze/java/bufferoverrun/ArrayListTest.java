/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.ArrayList;

class ArrayListTest {

  void alloc_is_negative_bad() {
    // initial capacity cannot be negative
    ArrayList<Integer> x = new ArrayList<Integer>(-1);
  }

  void alloc_is_ok() {
    // initial capacity cannot be negative
    ArrayList<Integer> x = new ArrayList<Integer>(9);
  }

  void add_in_loop_ok() {
    ArrayList<Integer> a = new ArrayList<>();
    for (int i = 0; i < 5; i++) {
      a.add(0);
    }
    int j = a.get(3);
  }

  void add_in_loop_bad() {
    ArrayList<Integer> a = new ArrayList<>();
    for (int i = 0; i < 5; i++) {
      a.add(0);
    }
    int j = a.get(6);
  }

  void add_in_loop_by_param_ok(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (int i = 0; i < b.size(); i++) {
      a.add(0);
    }
    int j = a.get(b.size() - 1);
  }

  void add_in_loop_by_param_bad(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (int i = 0; i < b.size(); i++) {
      a.add(0);
    }
    int j = a.get(b.size() + 1);
  }
}
