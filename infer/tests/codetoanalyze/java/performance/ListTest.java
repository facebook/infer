/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import com.google.common.base.Objects;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

class ListTest {

  int indexOfImpl_linear(List<?> list, Object element) {
    ListIterator<?> listIterator = list.listIterator();
    while (listIterator.hasNext()) {
      if (Objects.equal(element, listIterator.next())) {
        return listIterator.previousIndex();
      }
    }
    return -1;
  }

  void sort_comparator_nlogn(List<Person> people) {
    people.sort(new LexicographicComparator());
  }

  void sublist(List<String> filesList) {

    for (String file : filesList.subList(1, filesList.size())) {}
  }

  void sublist_constant(List<String> filesList) {

    for (String file : filesList.subList(1, 3)) {}
  }

  void asList_linear(String[] array) {
    List<String> list = Arrays.asList(array);
    for (String el : list) {}
  }

  boolean unknown_bool;

  List<Integer> two_lists(List<Integer> l1, List<Integer> l2) {
    List<Integer> l;
    if (unknown_bool) {
      l = l1;
    } else {
      l = l2;
    }
    return l;
  }

  void iterate_elements_linear(List<Integer> l) {
    Iterator iterator = l.iterator();
    while (iterator.hasNext()) {
      iterator.next();
    }
  }

  void call_iterate_elements_linear(List<Integer> l1, List<Integer> l2) {
    iterate_elements_linear(two_lists(l1, l2));
  }

  void iter_multiple_list1_linear(List<Integer> l1, List<Integer> l2) {
    List<Integer> l;
    if (unknown_bool) {
      l = l1;
    } else {
      l = l2;
    }
    List<Integer> m = new ArrayList<Integer>();
    for (Integer i : l) {
      m.add(i);
    }
  }

  void iter_multiple_list2_linear(List<Integer> l1, List<Integer> l2) {
    List<Integer> l;
    if (unknown_bool) {
      l = l1;
    } else {
      l = l2;
    }
    for (Integer i : l) {}
  }

  void iter_multiple_list3_linear(List<Integer> a, List<Integer> l1, List<Integer> l2) {
    List<Integer> l;
    if (unknown_bool) {
      l = l1;
    } else {
      l = l2;
    }
    a.addAll(l);
    for (Integer i : a) {}
  }
}
