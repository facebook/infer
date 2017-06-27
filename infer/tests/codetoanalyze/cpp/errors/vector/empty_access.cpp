/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <vector>

int access_empty() {
  const std::vector<int> vec;
  return vec[0];
}

int access_empty_front_bad() {
  const std::vector<int> vec;
  return vec.front();
}

int access_empty_back_bad() {
  const std::vector<int> vec;
  return vec.back();
}

int access_nonempty() {
  const std::vector<int> vec(1);
  return vec[0];
}

int clear_empty() {
  std::vector<int> vec(1);
  vec.clear();
  return vec[0];
}

int resize0_empty() {
  std::vector<int> vec(1);
  vec.resize(0);
  return vec[0];
}

int resize1_nonempty() {
  std::vector<int> vec;
  vec.resize(1);
  return vec[0];
}

int resize_n_nonempty(int n) {
  std::vector<int> vec;
  vec.resize(n);
  return vec[0];
}

int push_back_nonempty() {
  std::vector<int> vec;
  vec.push_back(1);
  return vec[0];
}

int copy_empty() {
  std::vector<int> vec1;
  std::vector<int> vec2 = vec1;
  return vec2[0];
}

int copy_nonempty() {
  std::vector<int> vec1(10);
  std::vector<int> vec2 = vec1;
  return vec2[0];
}

int assign_empty() {
  std::vector<int> vec1;
  std::vector<int> vec2(1);
  vec2 = vec1;
  return vec2[0];
}

int assign_nonempty() {
  std::vector<int> vec1(1);
  std::vector<int> vec2;
  vec2 = vec1;
  return vec2[0];
}

int empty_check_nonempty(std::vector<int>& vec) {
  if (vec.empty()) {
    return 1;
  }
  return vec[0];
}

int empty_check_nonempty2(std::vector<int>& vec) {
  if (vec.empty()) {
    vec.push_back(1);
  }
  return vec[0];
}

int empty_check_access_empty(std::vector<int>& vec) {
  if (vec.empty()) {
    return vec[0];
  }
  return 1;
}

int size_check0_empty(std::vector<int>& vec) {
  if (vec.size() == 0) {
    return vec[0];
  }
  return 1;
}

int size_check1_nonempty() {
  std::vector<int> vec;
  if (vec.size() > 0) {
    return vec[0];
  }
  return 1;
}

int vector_param_access(std::vector<int>& v) {
  return v.back(); // shouldn't report anything here
}

int vector_as_param_empty() {
  std::vector<int> v;
  return vector_param_access(v);
}

int vector_as_param_nonempty() {
  std::vector<int> v(1);
  return vector_param_access(v);
}

int vector_param_by_value_access(std::vector<int> v) {
  return v.back(); // shouldn't report anything here
}

int vector_as_param_by_value_empty() {
  std::vector<int> v;
  return vector_param_by_value_access(v);
}

void vector_param_by_value_clear(std::vector<int> v) { v.clear(); }

int vector_as_param_by_value_clear_no_crash() {
  std::vector<int> v(1);
  vector_param_by_value_clear(v);
  return v[0];
}

void vector_param_clear(std::vector<int>& v) { v.clear(); }

int vector_as_param_clear() {
  std::vector<int> v(1);
  vector_param_clear(v);
  return v[0];
}

std::vector<int> get_vector() {
  std::vector<int> x;
  return x;
}

int getter_empty() { return get_vector()[0]; }
