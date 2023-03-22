/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <array>

int std_array_bo_Bad() {
  std::array<int, 42> a;
  return a[42];
}

int normal_array_bo() {
  int b[42];
  return b[42];
}

void new_char_Good() {
  uint64_t len = 13;
  char* dst;
  dst = new char[len];
}

void new_int1_Bad() {
  uint64_t len = 4611686018427387903; // (1 << 62) - 1
  int32_t* dst;
  dst = new int32_t[len];
}

void new_int2_Bad() {
  uint64_t len = 9223372036854775807; // (1 << 63) - 1
  int32_t* dst;
  dst = new int32_t[len];
}

void new_int3_Bad() {
  uint64_t len = 18446744073709551615; // (1 << 64) - 1
  int32_t* dst;
  dst = new int32_t[len];
}

void std_array_contents_Good() {
  std::array<int, 10> a;
  a[0] = 5;
  a[a[0]] = 0;
}

void std_array_contents_Bad() {
  std::array<int, 10> a;
  a[0] = 10;
  a[a[0]] = 0;
}

void array_iter1_Good() {
  std::array<int, 11> a;
  for (auto it = a.begin(); it < a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter1_Bad() {
  std::array<int, 5> a;
  for (auto it = a.begin(); it < a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter2_Good() {
  std::array<int, 11> a;
  for (auto it = a.begin(); it != a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter2_Bad() {
  std::array<int, 5> a;
  for (auto it = a.begin(); it != a.end(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_iter3_Good() {
  std::array<int, 11> a = {10};
  for (auto it = a.cbegin(); it < a.cend(); ++it) {
    a[*it] = 10;
  }
}

void array_iter3_Bad() {
  std::array<int, 5> a = {10};
  for (auto it = a.cbegin(); it < a.cend(); ++it) {
    a[*it] = 10;
  }
}

void array_iter_front_Good() {
  std::array<int, 11> a;
  a.front() = 10;
  a[a[0]] = 0;
}

void array_iter_front_Bad() {
  std::array<int, 5> a;
  a.front() = 10;
  a[a[0]] = 0;
}

void array_iter_back_Good() {
  std::array<int, 11> a;
  a.back() = 10;
  a[a[0]] = 0;
}

void array_iter_back_Bad() {
  std::array<int, 5> a;
  a.back() = 10;
  a[a[0]] = 0;
}

void array_rev_iter_Good() {
  std::array<int, 11> a;
  for (auto it = a.rbegin(); it < a.rend(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void array_rev_iter_Bad_FN() {
  std::array<int, 5> a;
  for (auto it = a.rbegin(); it < a.rend(); ++it) {
    *it = 10;
  }
  a[a[0]] = 0;
}

void malloc_zero_Bad() { int* a = (int*)malloc(sizeof(int) * 0); }

void new_array_zero_Good() { int* a = new int[0]; }
