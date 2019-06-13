/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
import com.google.common.base.Objects;
import java.util.Arrays;
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
}
