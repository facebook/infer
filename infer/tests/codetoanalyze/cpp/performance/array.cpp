/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <algorithm>
#include <iostream>
#include <iterator>

void array_access_constant() {
  float radii[8];
  for (int i = 0; i < 4; ++i) {
    radii[i * 2] = radii[i];
    radii[i * 2 + 1] = radii[i] + 1;
  }
}

void array_access_overrun_constant() {
  float radii[8];
  for (int i = 0; i < 4; ++i) {
    radii[i * 2] = radii[i];
    radii[i * 2 + 2] = radii[i] + 1;
  }
}

void array_access_weird_linear(long (&optionNumerators)[], size_t length) {
  for (int j = 0; j < length; ++j) {
    if (10 < optionNumerators[j] + 1) {
    }
  }
}

bool binary_search_log_FN(std::string (&arr)[], size_t length) {
  return std::binary_search(arr, arr + length, "x");
}

void fill_linear_FN(std::string (&arr)[], size_t length) {
  std::fill(arr, arr + length, "x");
}

void copy_linear_FN(std::string (&arr1)[],
                    std::string (&arr2)[],
                    size_t arr1_length) {
  std::copy(arr1, arr1 + arr1_length, arr2);
}

void copy_for_linear(std::string (&arr1)[],
                     std::string (&arr2)[],
                     size_t arr1_length) {
  for (int i = 0; i < arr1_length; ++i) {
    arr2[i] = arr1[i];
  }
}

// sort_array_template does not exist in the cost-issues file.
// Clang frontend only creates procedures
// when the template is instantiated.
template <size_t N>
void sort_array_template_nlogn(const int (&arr1)[N]) {
  std::sort(std::begin(arr1), std::end(arr1));
}

void sort_array_inst_constant() {
  int arr1[300];
  std::sort(std::begin(arr1), std::end(arr1));
}

void sort_array_nlogn_FN(size_t length) {
  std::string arr1[length];
  std::sort(arr1, arr1 + length);
}

// expected: O(N)
void array_loop_linear_FN(int (&N)[]) {
  int i = 0;
  while (i < sizeof(*N) / sizeof(N[0])) {
    i++;
  }
}

// expected: O(N)
void array_loop_while_while_linear_FN(int (&N)[]) {
  int i = 0;
  while (i < sizeof(*N) / sizeof(N[0])) {
    i++;
  }
  int j = 0;
  while (j < sizeof(*N) / sizeof(N[0])) {
    j++;
  }
}

// expected: O(N+M)
void array_loop_while_for_linear_FN(int (&N)[], int (&M)[]) {
  int i = 0;
  while (i < sizeof(*N) / sizeof(N[0])) {
    i++;
  }
  for (int j = 0; j < sizeof(*M) / sizeof(M[0]); j++) {
  }
}

// expected: O(N*M)
void array_nested_loop_while_for_FN(int (&N)[], int (&M)[]) {
  int i = 0;
  while (i < sizeof(*N) / sizeof(N[0])) {
    for (int j = 0; j < sizeof(*M) / sizeof(M[0]); j++) {
    }
    i++;
  }
}

int size = 10;

void loop_global_var_size_constant() {
  int M[size];
  for (int j = 0; j < sizeof(*M) / sizeof(M[0]); j++) {
  }
}

// expected: O(N)
void array_nested_loop_while_for_linear_FN(int (&N)[]) {
  int i = 0;
  while (i < sizeof(*N) / sizeof(N[0])) {
    loop_global_var_size_constant();
    i++;
  }
}

// expected: O(N*M)
void array_nested_loop_for_FN(int (&N)[], int m) {
  int i = 0;
  while (i < sizeof(*N) / sizeof(N[0])) {
    for (int j = 0; j < m; j++) {
    }
    i++;
  }
}
