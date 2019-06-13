/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import java.util.ArrayList;
import javax.inject.*;

class HoistModeled {

  @Inject private Provider<Integer> mProvider;

  void expensive_get_hoist(int size) {
    for (int i = 0; i < size; i++) {
      mProvider.get();
    }
  }

  void linear_contains_hoist(ArrayList<Integer> list, Integer el) {
    int count = 0;
    for (int i = 0; i < 10; i++) {
      if (list.contains(el)) {
        count++;
      }
    }
  }

  void constant_contains_dont_hoist(Integer el) {
    boolean contains = false;
    ArrayList<Integer> mylist = new ArrayList<Integer>();
    mylist.add(1);
    for (int i = 0; i < 10; i++) {
      contains = mylist.contains(el);
    }
  }

  void constant_substring_dont_hoist(String s) {
    String sub;
    for (int i = 0; i < 10; i++) {
      sub = s.substring(2, 10);
    }
  }

  void linear_substring_hoist(String s, ArrayList<Integer> list, Integer el) {
    String sub;
    int length = s.length();
    for (int i = 0; i < 10; i++) {
      sub = s.substring(2, length - 1);
    }
    for (int i = 0; i < 10; i++) {
      sub = s.substring(1);
    }
  }

  void call_expensive_hoist(String s, ArrayList<Integer> list, Integer el) {
    for (int i = 0; i < 10; i++) {
      expensive_get_hoist(10);
    }
  }

  void expensive_get_hoist_hoist_me(String s, ArrayList<Integer> list, Integer el) {
    String sub;
    int length = s.length();
    for (int i = 0; i < 10; i++) {
      call_expensive_hoist("ez", list, el);
    }
  }
}
