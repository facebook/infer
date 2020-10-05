/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>
#include <vector>

void iterator_read_after_emplace_bad(std::vector<int>& vec) {
  auto iter = vec.begin();
  vec.emplace(iter, 4);
  std::cout << *iter << '\n';
}

void iterator_next_after_emplace_bad(std::vector<int>& vec) {
  auto iter = vec.begin();
  vec.emplace(iter, 4);
  ++iter;
  std::cout << *iter << '\n';
}

void another_iterator_ok(std::vector<int>& vec) {
  auto iter = vec.begin();
  vec.emplace(iter, 4);
  auto another_iter = vec.begin();
  std::cout << *another_iter << '\n';
  ++another_iter;
  std::cout << *another_iter << '\n';
}

void read_iterator_loop_ok(std::vector<int>& vec) {
  int sum = 0;
  for (auto iter = vec.begin(); iter != vec.end(); ++iter) {
    sum += *iter;
  }
}

void iterator_next_after_emplace_loop_latent(std::vector<int>& vec) {
  int sum = 0;
  for (auto iter = vec.begin(); iter != vec.end(); ++iter) {
    int elem = *iter;
    sum += elem;
    if (elem < 0)
      vec.emplace(iter, -elem);
  }
}

void iterator_after_push_back_loop_latent(std::vector<int>& vec_other) {
  std::vector<int> vec(2);
  auto iter_begin = vec.begin();
  auto iter_end = vec.end();
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
  int sum = 0;
  for (auto iter = iter_begin; iter != iter_end; ++iter) {
    sum += *iter;
  }
}

void FN_iterator_empty_vector_read_bad() {
  std::vector<int> vec = {};
  auto iter = vec.begin();
  std::cout << *iter << '\n';
}

void iterator_end_read_bad() {
  std::vector<int> vec = {1, 2};
  auto iter = vec.end();
  std::cout << *iter << '\n';
}

void iterator_end_next_bad() {
  std::vector<int> vec = {1, 2};
  auto iter = vec.end();
  ++iter;
}

void iterator_end_prev_read_ok() {
  std::vector<int> vec = {1, 2};
  auto iter = vec.end();
  std::cout << *(--iter) << '\n';
}

void iterator_prev_after_emplace_bad(std::vector<int>& vec) {
  auto iter = vec.begin();
  ++iter;
  vec.emplace(iter, 4);
  --iter;
  std::cout << *iter << '\n';
}

void FN_iterator_begin_prev_read_bad() {
  std::vector<int> vec = {1, 2};
  auto iter = vec.begin();
  std::cout << *(--iter) << '\n';
}

bool for_each_ok(std::vector<int>& vec, bool b) {
  int res = 0;
  for (const auto& elem : vec) {
    res += 0;
  }
  return b;
}

void call_iterator_loop_ok(bool b) {
  std::vector<int> vec;
  bool finished = false;
  while (!finished) {
    if (!for_each_ok(vec, b))
      return;
  }
}

std::vector<int>::iterator find(std::vector<int>& vec, bool b) {
  for (auto it = vec.begin(); it != vec.end(); ++it) {
    if (b) {
      return it;
    }
  }
  return vec.end();
}

void iterator_end_returned_ok(std::vector<int> vec, bool b) {
  auto it = find(vec, b);
  if (it != vec.end()) {
    *it = 3;
  } else {
    return;
  }
}
