/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <list>
#include <string>

void iterate_over_list_local_it_linear(std::list<int> mlist) {
  std::list<int>::iterator it;
  for (it = mlist.begin(); it != mlist.end(); it++) {
  }
}

void iterate_over_list_linear(std::list<int> mlist) {
  for (auto it = mlist.begin(); it != mlist.end(); it++) {
  }
}

void iteratec_over_list_linear(std::list<int> mlist) {
  for (auto it = mlist.cbegin(); it != mlist.cend(); it++) {
  }
}

void iterate_rev_over_list_linear(std::list<int> mlist) {
  for (auto it = mlist.rbegin(); it != mlist.rend(); ++it) {
  }
}

void iteratec_rev_over_list_linear(std::list<int> mlist) {
  for (auto it = mlist.crbegin(); it != mlist.crend(); ++it) {
  }
}

void iterate_over_list_for_linear(std::list<int> mlist) {
  for (const int& el : mlist) {
  }
}

void iterate_over_local_list_linear(std::list<int> mlist) {
  std::list<int> local_list = mlist;
  std::list<int>::iterator it;
  for (it = local_list.begin(); it != local_list.end(); it++) {
  }
}

void list_push_back_constant() {
  std::list<int> list;
  list.push_back(42);
}

void list_insert_constant() {
  std::list<int> list;
  std::list<int>::iterator it;
  it = list.begin();
  list.insert(it, 42);
}

void list_add_in_loop_constant() {
  std::list<int> list;
  for (int i = 0; i < 10; ++i) {
    list.push_back(i);
  }
  for (int i = 0, size = list.size(); i < size; ++i) {
  }
}

void list_add_in_nested_loop_constant() {
  for (int j = 0; j < 10; j++) {
    list_add_in_loop_constant();
  }
}

void list_push_back_then_loop_constant() {
  std::list<int> list;
  list.push_back(0);
  list.push_back(1);
  list.push_back(2);
  list.push_back(3);
  list.push_back(4);
  list.push_back(5);

  for (int i = 0, size = list.size(); i < size; ++i) {
  }
}

void list_get_underrun_constant() {
  std::list<int> list;
  auto l_front = list.begin();
  std::advance(l_front, 0);
}

void list_get_overrun_constant() {
  std::list<int> list;
  list.push_back(0);
  auto l_front = list.begin();
  std::advance(l_front, 2);
}

void list_get_constant() {
  std::list<int> list;
  list.push_back(0);
  list.push_back(1);
  list.push_back(2);
  auto l_front = list.begin();

  for (int i = 0, size = list.size(); i < size; ++i) {
    std::advance(l_front, i);
  }
}

void sort_list_int_nlogn_FN(std::list<int> mylist) { mylist.sort(); }

void sort_list_string_constant() {
  std::list<std::string> mylist;

  mylist.push_back("one");
  mylist.push_back("two");
  mylist.push_back("Three");

  mylist.sort();
}

bool compare_nocase_linear(const std::string& first,
                           const std::string& second) {
  unsigned int i = 0;
  while ((i < first.length()) && (i < second.length())) {
    if (tolower(first[i]) < tolower(second[i]))
      return true;
    else if (tolower(first[i]) > tolower(second[i]))
      return false;
    ++i;
  }
  return (first.length() < second.length());
}

void sort_list_string_no_case_constant() {
  std::list<std::string> mylist;

  mylist.push_back("one");
  mylist.push_back("two");
  mylist.push_back("Three");

  mylist.sort(compare_nocase_linear);
}

void sort_list_string_no_case_nlogn_FN(std::list<std::string> list2) {
  std::list<std::string> mylist;

  for (std::string x : list2) {
    mylist.push_back(x);
  }

  mylist.sort(compare_nocase_linear);
}

// Expected: O(list1 x list2); got list1
// more details here: T104735254
std::list<int> remove_duplicates_quadratic_FN(std::list<int>& list1) {
  std::list<int> list2;

  for (int x : list1) {
    bool found = false;
    for (int y : list2)
      if (x == y)
        found = true;
    if (!found)
      list2.push_back(x);
  }

  return list2;
}
