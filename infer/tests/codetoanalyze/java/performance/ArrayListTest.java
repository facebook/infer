/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

public class ArrayListTest {

  public void iterate_over_arraylist(ArrayList<Integer> list) {
    for (int i = 0, size = list.size(); i < size; ++i) {}
  }

  public void iterate_over_local_arraylist(ArrayList<Integer> list) {
    ArrayList<Integer> local_list = list;
    for (int i = 0, size = local_list.size(); i < size; ++i) {}
  }

  public void arraylist_empty_underrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(-1, 42);
  }

  public void arraylist_empty_ok_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0, 42);
  }

  public void arraylist_empty_overrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(1, 42);
  }

  public void arraylist_add3_overrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(42);
    list.add(1337);
    list.add(1984);
    list.add(4, 666);
  }

  public void arraylist_add_in_loop_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    for (int i = 0; i < 10; ++i) {
      list.add(i);
    }
    for (int i = 0, size = list.size(); i < size; ++i) {}
  }

  public void arraylist_add_in_nested_loop_constant_constant() {
    for (int j = 0; j < 10; j++) {
      ArrayList<Integer> list = new ArrayList<Integer>();
      for (int i = 0; i < 10; ++i) {
        list.add(i);
      }
      for (int i = 0, size = list.size(); i < size; ++i) {}
    }
  }

  public void arraylist_add_then_loop_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.add(1);
    list.add(2);
    list.add(3);
    list.add(4);
    list.add(5);
    list.add(6);
    list.add(7);
    list.add(8);
    list.add(9);
    list.add(10);
    list.add(11);
    list.add(12);
    list.add(13);
    list.add(14);
    list.add(15);

    for (int i = 0, size = list.size(); i < size; ++i) {}
  }

  public void arraylist_addAll_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(2);
    list.add(3);

    ArrayList<Integer> list2 = new ArrayList<Integer>();
    list2.add(0);
    list2.add(1);

    list2.addAll(0, list);
    list2.addAll(5, list);
  }

  public void arraylist_get_underrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.get(0);
  }

  public void arraylist_get_overrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.get(2);
  }

  public void arraylist_get_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.add(1);
    list.add(2);

    for (int i = 0, size = list.size(); i < size; ++i) {
      list.get(i);
    }
  }

  public void arraylist_set_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.add(1);
    list.add(2);
    for (int i = 0, size = list.size(); i < size; ++i) {
      list.set(i, i);
    }
  }

  public void arraylist_set_underrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.set(0, 10);
  }

  public void arraylist_set_overrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.set(1, 10);
  }

  public void arraylist_remove_overrun_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.remove(1);
  }

  public void arraylist_remove_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(0);
    list.add(1);
    list.remove(0);
    list.get(0);
  }

  public void arraylist_remove_in_loop_constant() {
    ArrayList<Integer> list = new ArrayList<Integer>();
    for (int i = 0; i < 10; ++i) {
      list.add(i);
    }
    for (int i = 0, size = list.size(); i < size; ++i) {
      list.remove(i);
    }
  }

  public void iterate_with_iterator(ArrayList<Integer> list) {
    for (Integer element : list) {}
  }

  public void iterate_while_has_next(ArrayList<Integer> list) {
    Iterator itr = list.iterator();

    while (itr.hasNext()) {
      System.out.println(itr.next());
    }
  }

  public void iterate_over_arraylist_with_inner(ArrayList<Integer> list1) {
    ArrayList<Integer> list2 = new ArrayList<Integer>();

    for (Integer element : list1) {
      list2.size();
    }
  }

  // Control vars include the list length and the element which is some intValue.
  // O(list.length x (-list.elements + 11))
  // Simplified version of real code https://fburl.com/a3gge1b7
  public boolean iterate_over_arraylist_shortcut_linear(ArrayList<Integer> list) {
    for (Integer element : list) {
      if (element > 10) {
        return false;
      }
    }
    return true;
  }

  public static void sortArrayList(ArrayList<Integer> list) {
    java.util.Collections.sort(list);
  }

  private static void call_sortArrayList(ArrayList<Integer> list) {
    sortArrayList(list);
  }

  private ArrayList<String> list = new ArrayList<>();

  // Since we know the maximum value of boolean [iterator.next().equals(s)] is 1, it is better to be
  // excluded from the cost.
  // Simplified version of real code https://fburl.com/traceview/tpc0grh2
  public boolean remove_string_from_list(String s) {
    Iterator<String> iterator = list.iterator();
    while (iterator.hasNext()) {
      if (iterator.next().equals(s)) {
        iterator.remove();
        return true;
      }
    }
    return false;
  }

  void constructor_linear(ArrayList<String> list) {
    ArrayList<String> slist = new ArrayList<>(list);
    for (int i = 0; i < slist.size(); i++) {}
  }

  void constructor_add_all(ArrayList<String> list, ArrayList<String> l) {
    ArrayList<String> slist = new ArrayList<>(list);
    slist.addAll(l); // increments the size of slist by l.length
    for (int i = 0; i < list.size(); i++) {}
  }

  void constructor_add_all_sym(ArrayList<String> list, ArrayList<String> l) {
    ArrayList<String> slist = new ArrayList<>(list);
    list.addAll(l); // increments the size of list by l.length
    for (int i = 0; i < slist.size(); i++) {}
  }

  void add_all_linear(ArrayList<String> l1, ArrayList<String> l2) {
    l1.addAll(l2);
  }

  void sort_comparator_nlogn(ArrayList<Person> people) {
    java.util.Collections.sort(people, new LexicographicComparator());
  }

  Person max_linear(ArrayList<Person> people) {
    return java.util.Collections.max(people, new LexicographicComparator());
  }

  void empty_list_constant(int k) {
    // create an empty list with initial capacity k, which is ignored
    ArrayList<Integer> x = new ArrayList<Integer>(k);
    for (int i = 0; i < x.size(); i++) {}
  }

  void json_array_constructor_linear(ArrayList<Integer> arr) {
    try {
      org.json.JSONArray jArray = new org.json.JSONArray(arr);
      for (int i = 0; i < jArray.length(); i++) {}
    } catch (Exception e) {

    }
  }

  void linear(int i, ArrayList<Integer> a) {
    while (a.size() >= i) {
      a.remove(0);
    }
  }

  class Elt {
    boolean b;

    public boolean get_boolean() {
      return b;
    }
  }

  ArrayList<Elt> arr = new ArrayList<Elt>();

  void boolean_control_var_linear() {
    for (int i = 0; i < arr.size(); i++) {
      if (!arr.get(i).get_boolean()) {
        break;
      }
    }
  }

  public static HashMap<Integer, Integer> init_with_put_linear(ArrayList<Integer> a) {
    HashMap<Integer, Integer> m = new HashMap<>();
    for (Integer i : a) {
      m.put(i, i);
    }
    return m;
  }

  public static void call_init_with_put_linear(ArrayList<Integer> a) {
    HashMap<Integer, Integer> m = init_with_put_linear(a);
    for (HashMap.Entry<Integer, Integer> e : m.entrySet()) {}
  }

  boolean unknown_bool;

  void id(ArrayList<Integer> a) {
    a.add(0);
    a.remove(0);
  }

  void substitute_array_block_linear(ArrayList<Integer> a, ArrayList<Integer> b) {
    ArrayList<Integer> c;
    if (unknown_bool) {
      c = a;
    } else {
      c = b;
    }
    id(c);
    iterate_over_arraylist(a);
  }

  void array_get_elem_constant() {
    ArrayList<Integer> a = new ArrayList<>();
    a.add(5);
    for (int i = 0; i < a.get(0); i++) {}
  }

  boolean rand;

  int get_size(ArrayList<Integer> arr) {
    return arr.size();
  }

  void loop_invariant_linear(ArrayList<Integer> arr, InputStream is) {
    if (rand) {
      arr = new ArrayList<>();
    }
    for (int i = 0; i < get_size(arr); i++) {
      try {
        is.read();
      } catch (Exception e) {
      }
    }
  }
}

class LexicographicComparator implements java.util.Comparator<Person> {
  @Override
  public int compare(Person a, Person b) {
    return a.name.compareToIgnoreCase(b.name);
  }
}

class Person {

  String name;
  int age;

  Person(String n, int a) {
    name = n;
    age = a;
  }
}
