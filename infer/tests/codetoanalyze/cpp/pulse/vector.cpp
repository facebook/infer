/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <iostream>
#include <vector>

void deref_vector_element_after_push_back_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  int* y = elt;
  vec.push_back(42);
  std::cout << *y << "\n";
}

void deref_local_vector_element_after_push_back_bad() {
  std::vector<int> vec = {0, 0};
  int* elt = &vec[1];
  vec.push_back(42);
  std::cout << *elt << "\n";
}

void two_push_back_ok(std::vector<int>& vec) {
  vec.push_back(32);
  vec.push_back(52);
}

void push_back_in_loop_ok(std::vector<int>& vec, std::vector<int>& vec_other) {
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
}

void reserve_then_push_back_ok(std::vector<int>& vec) {
  vec.reserve(vec.size() + 1);
  int* elt = &vec[1];
  vec.push_back(42);
  std::cout << *elt << "\n";
}

void FN_reserve_too_small_bad() {
  std::vector<int> vec;
  vec.reserve(1);
  vec.push_back(32);
  int* elt = &vec[0];
  vec.push_back(52);
  std::cout << *elt << "\n";
}

void reserve_then_push_back_loop_ok(std::vector<int>& vec,
                                    std::vector<int>& vec_other) {
  vec.reserve(vec.size() + vec_other.size());
  int* elt = &vec[1];
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
  std::cout << *elt << "\n";
}

void FP_init_fill_then_push_back_loop_ok(std::vector<int>& vec_other) {
  std::vector<int> vec(vec_other.size());
  int* elt = &vec[1];
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
  std::cout << *elt << "\n";
}

void push_back_loop_bad(std::vector<int>& vec_other) {
  std::vector<int> vec(2);
  int* elt = &vec[1];
  for (const auto& i : vec_other) {
    vec.push_back(i);
  }
  std::cout << *elt << "\n";
}

void reserve_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.reserve(vec.size() + 1);
  std::cout << *elt << "\n";
}

void clear_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.clear();
  std::cout << *elt << "\n";
}

void assign_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.assign(11, 7);
  std::cout << *elt << "\n";
}

void shrink_to_fit_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.shrink_to_fit();
  std::cout << *elt << "\n";
}

void insert_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.insert(vec.begin(), 7);
  std::cout << *elt << "\n";
}

void emplace_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.emplace(vec.begin(), 7);
  std::cout << *elt << "\n";
}

void emplace_back_bad(std::vector<int>& vec) {
  int* elt = &vec[1];
  vec.emplace_back(7);
  std::cout << *elt << "\n";
}

void f(int&);

void push_back_value_ok(std::vector<int>& vec) {
  int x = vec[0];
  vec.push_back(7);
  f(x);
}

struct VectorA {
  int x;

  void push_back_value_field_ok(std::vector<int>& vec) {
    x = vec[0];
    vec.push_back(7);
    f(x);
  }
};
