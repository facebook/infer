/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <algorithm>
#include <list>
#include <map>
#include <vector>
#include <iostream>

int add(int x, int y) { return x + y; }

int loop_linear_list(int x, const std::list<int>& list) {
  int sum = 0;
  for (auto&& el : list) {
    sum = +el + x;
  }
  return sum;
}

int loop_linear_vec(int x, const std::vector<int>& vec) {
  int sum = 0;
  for (auto&& el : vec) {
    sum = +el + x;
  }
  return sum;
}

void map_linear_FN(std::map<int, int>& map) {
  for_each(map.begin(), map.end(), [](std::pair<int, int> el) {
    add(el.first, el.second);
  });
}

// FN: We have limited lambda support and canot incur costs of the lambda calls
// yet.
void list_quadratic_FN(std::list<int>& mylist) {
  for_each(mylist.begin(), mylist.end(), [mylist](int el) {
    loop_linear_list(el, mylist);
  });
}

void vector_quadratic_FN(std::vector<int>& vec) {
  for_each(vec.begin(), vec.end(), [vec](int el) { loop_linear_vec(el, vec); });
}
