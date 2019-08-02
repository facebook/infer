/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <vector>

int FN_access_empty_bad() {
  const std::vector<int> vec;
  return vec[0];
}

int FN_access_empty_front_bad() {
  const std::vector<int> vec;
  return vec.front();
}

int FN_access_empty_back_bad() {
  const std::vector<int> vec;
  return vec.back();
}

int access_nonempty_ok() {
  const std::vector<int> vec(1);
  return vec[0];
}

int FN_clear_empty_bad() {
  std::vector<int> vec(1);
  vec.clear();
  return vec[0];
}

int FN_resize0_empty_bad() {
  std::vector<int> vec(1);
  vec.resize(0);
  return vec[0];
}

int resize1_nonempty_ok() {
  std::vector<int> vec;
  vec.resize(1);
  return vec[0];
}

int resize_n_nonempty_ok(int n) {
  std::vector<int> vec;
  vec.resize(n);
  return vec[0];
}

int push_back_nonempty_ok() {
  std::vector<int> vec;
  vec.push_back(1);
  return vec[0];
}

int FN_copy_empty_bad() {
  std::vector<int> vec1;
  std::vector<int> vec2 = vec1;
  return vec2[0];
}

int copy_nonempty_ok() {
  std::vector<int> vec1(10);
  std::vector<int> vec2 = vec1;
  return vec2[0];
}

int FN_assign_empty_bad() {
  std::vector<int> vec1;
  std::vector<int> vec2(1);
  vec2 = vec1;
  return vec2[0];
}

int assign_nonempty_ok() {
  std::vector<int> vec1(1);
  std::vector<int> vec2;
  vec2 = vec1;
  return vec2[0];
}

int empty_check_nonempty_ok(std::vector<int>& vec) {
  if (vec.empty()) {
    return 1;
  }
  return vec[0];
}

int empty_check_nonempty2_ok(std::vector<int>& vec) {
  if (vec.empty()) {
    vec.push_back(1);
  }
  return vec[0];
}

int FN_empty_check_access_empty_bad(std::vector<int>& vec) {
  if (vec.empty()) {
    return vec[0];
  }
  return 1;
}

int FN_size_check0_empty_bad(std::vector<int>& vec) {
  if (vec.size() == 0) {
    return vec[0];
  }
  return 1;
}

int size_check1_nonempty_ok() {
  std::vector<int> vec;
  if (vec.size() > 0) {
    return vec[0];
  }
  return 1;
}

int vector_param_access(std::vector<int>& v) {
  return v.back(); // shouldn't report anything here
}

// Cannot_star
int ERROR_vector_as_param_empty_bad() {
  std::vector<int> v;
  return vector_param_access(v);
}

// Cannot_star
int ERROR_vector_as_param_nonempty_ok() {
  std::vector<int> v(1);
  return vector_param_access(v);
}

int vector_param_by_value_access(std::vector<int> v) {
  return v.back(); // shouldn't report anything here
}

// Cannot_star
int ERROR_vector_as_param_by_value_empty_bad() {
  std::vector<int> v;
  return vector_param_by_value_access(v);
}

void vector_param_by_value_clear(std::vector<int> v) { v.clear(); }

int ERROR_vector_as_param_by_value_clear_ok() {
  std::vector<int> v(1);
  vector_param_by_value_clear(v);
  return v[0];
}

void vector_param_clear(std::vector<int>& v) { v.clear(); }

// Cannot_star
int FN_vector_as_param_clear_bad() {
  std::vector<int> v(1);
  vector_param_clear(v);
  return v[0];
}

std::vector<int> get_vector() {
  std::vector<int> x;
  return x;
}

int FN_getter_empty_bad() { return get_vector()[0]; }
