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

  boolean unknown_bool;

  void add_in_loop_by_param2_ok(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (int i = 0; i < b.size(); i++) {
      if (unknown_bool) {
        a.add(0);
      }
    } // a.size should be [0, b.size]
    if (a.size() > 0) {
      int j = b.get(a.size() - 1);
    }
  }

  void add_in_loop_by_param2_bad(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (int i = 0; i < b.size(); i++) {
      if (unknown_bool) {
        a.add(0);
      }
    } // a.size should be [0, b.size]
    int j = b.get(a.size());
  }

  void add_in_loop_iterator_ok(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (Integer i : b) {
      a.add(i);
    }
    int j = a.get(b.size() - 1);
  }

  void add_in_loop_iterator_bad(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (Integer i : b) {
      a.add(i);
    }
    int j = a.get(b.size() + 1);
  }

  void remove_in_loop_iterator_good_FP(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (Integer i : b) {
      a.add(i);
    }
    for (Integer i : b) {
      a.remove(i);
    }
    /* a.size should be 0, but it is analyzed to [-oo, b.size] for now.
    - array smashing: It abstracts all members as one abstract value, so cannot precisely analyze
      the set of members in the array.
    - imprecise remove model: Even with the array smashing, it should have been able to analyze
      as [0, b.size], if the semantics of the model was preciser. */
    if (a.size() < 0) {
      int j = b.get(b.size());
    }
  }

  void remove_in_loop_iterator_bad(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (Integer i : b) {
      a.add(i);
    }
    for (Integer i : b) {
      a.remove(i);
    } // a.size should be 0
    int j = a.get(0);
  }

  void add_in_loop_iterator2_ok(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (Integer i : b) {
      if (unknown_bool) {
        a.add(i);
      }
    } // a.size should be [0, b.size]
    if (a.size() > 0) {
      int j = b.get(a.size() - 1);
    }
  }

  void add_in_loop_iterator2_bad(ArrayList<Integer> b) {
    ArrayList<Integer> a = new ArrayList<>();
    for (Integer i : b) {
      if (unknown_bool) {
        a.add(i);
      }
    } // a.size should be [0, b.size]
    int j = b.get(a.size());
  }

  void add_and_remove_ok(ArrayList<Integer> a) {
    ArrayList<Integer> b = new ArrayList<Integer>();
    b.add(0);
    for (Integer i : a) {
      b.add(0);
      b.remove(0);
    } // b.size is one here
    int j = b.get(0);
  }

  void add_and_remove_bad(ArrayList<Integer> a) {
    ArrayList<Integer> b = new ArrayList<Integer>();
    for (Integer i : a) {
      b.add(0);
      b.remove(0);
    } // b.size is zero here
    int j = b.get(0);
  }
}
