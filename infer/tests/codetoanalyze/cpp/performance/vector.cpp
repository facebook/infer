/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <algorithm>
#include <vector>
#include <iostream>

bool binary_search_log(std::vector<std::string>& vec) {
  return std::binary_search(vec.begin(), vec.end(), "x");
}

void iterate_over_vec_linear(std::vector<std::string>& vec) {
  for (auto it = vec.begin(); it != vec.end(); ++it) {
  }
}

void iteratec_over_vec_linear(std::vector<std::string>& vec) {
  for (auto it = vec.cbegin(); it != vec.cend(); ++it) {
  }
}

void iterate_rev_over_vec_linear(std::vector<std::string>& vec) {
  for (auto it = vec.rbegin(); it != vec.rend(); ++it) {
  }
}

void iteratec_rev_over_vec_linear(std::vector<std::string>& vec) {
  for (auto it = vec.crbegin(); it != vec.crend(); ++it) {
  }
}

void loop_over_vec_linear(std::vector<std::string>& vec) {
  for (auto i : vec) {
  }
}

void copy_iterative_method_linear(std::vector<std::string>& vec1) {
  std::vector<std::string> vec2;
  for (int i = 0; i < vec1.size(); i++)
    vec2.push_back(vec1[i]);
}

void copy_iterative_method_constant(std::vector<std::string>& vec1) {
  std::vector<std::string> vec2;
  for (int i = 0; i < 4; i++)
    vec2.push_back(vec1[i]);
}

void copy_assignment_operator_linear_FN(std::vector<std::string>& vec1) {
  std::vector<std::string> vec2;
  vec2 = vec1;
}

void copy_contructor_linear_FN(std::vector<std::string>& vec1) {
  std::vector<std::string> vec2(vec1);
}

void copy_inbuild_func_linear_FN(std::vector<std::string>& vec1) {
  std::vector<std::string> vec2;
  std::copy(vec1.begin(), vec1.end(), back_inserter(vec2));
}

void copy_assign_linear_FN(std::vector<std::string>& vec1) {
  std::vector<std::string> vec2;
  vec2.assign(vec1.begin(), vec1.end());
}

void copy_n_linear_FN(std::vector<std::string>& vec1, int n) {
  std::vector<std::string> vec2;
  copy_n(vec1.begin(), n, vec2.begin());
}

void copy_n_constant(std::vector<std::string>& vec1, int n) {
  std::vector<std::string> vec2;
  copy_n(vec1.begin(), 4, vec2.begin());
}

void iterate_vector_linear(std::vector<std::string>& vec) {
  for (int i = 0; i < vec.size(); i++) {
  }
}

std::vector<int>::iterator find_in_vector(std::vector<int>& vec) {
  return std::find(vec.begin(), vec.end(), 30);
}

void iterate_two_vectors_linear(std::vector<std::string>& vec1,
                                std::vector<std::string>& vec2) {
  for (int i = 0; i < vec1.size(); i++) {
  }

  for (int i = 0; i < vec2.size(); i++) {
  }
}

void iterate_vector_quadratic(std::vector<std::string>& vec) {
  for (int i = 0; i < vec.size(); i++) {
    iterate_vector_linear(vec);
  }
}

void iterate_vector_constant_times_linear(std::vector<std::string>& vec) {
  for (int i = 0; i < 5; i++) {
    iterate_vector_linear(vec);
  }
}

void iterate_vector_auto_linear(std::vector<int>& vec) {
  for (auto& v : vec)
    v *= v;
}

void sort_vector_nlogn(std::vector<int>& vec) {
  std::sort(vec.begin(), vec.end());
}

void insert_vector_constant(std::vector<int>& vec) {
  std::vector<int>::iterator it;

  it = vec.begin();
  it = vec.insert(it, 100);
  vec.insert(it, 1, 300);
}

void insert_new_vector_constant(std::vector<int>& vec) {
  std::vector<int>::iterator it;

  it = vec.begin();
  it = vec.insert(it, 100);

  std::vector<int> vec2(2, 400);
  vec.insert(it + 1, vec2.begin(), vec2.end());
}

void insert_new_vector_loop_linear(std::vector<int>& vec, int n) {
  std::vector<int>::iterator it;
  it = vec.begin();

  for (int i = 0; i < n; i++) {
    vec.insert(it, n);
  }
}

void insert_new_vector_loop_constant(std::vector<int>& vec) {
  std::vector<int>::iterator it;
  it = vec.begin();

  vec.push_back(42);
  vec.push_back(1337);
  for (int i = 0; i < 5; i++) {
    vec.insert(it, i);
  }
}

// Expected: O(vec1 x vec1);
// got O(vec1 x vec2) incorrectly
std::vector<int> remove_duplicates_quadratic(std::vector<int>& vec1) {
  std::vector<int> vec2;

  for (int x : vec1) {
    bool found = false;
    for (int y : vec2)
      if (x == y)
        found = true;
    if (!found)
      vec2.push_back(x);
  }

  return vec2;
}
