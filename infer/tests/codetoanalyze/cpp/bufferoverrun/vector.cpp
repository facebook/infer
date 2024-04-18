/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <vector>
#include <list>
#include <assert.h>

void out_of_bound_Bad(std::vector<int> v) {
  unsigned int n = v.size();
  v[n] = 1;
}

void simple_size_Good() {
  std::vector<int> v(3);
  v[v.size() - 1] = 2;
}

void simple_size_Bad() {
  std::vector<int> v(3);
  v[v.size()] = 2;
}

void constructor_overload1_Good() {
  std::vector<int> v(1);
  v[0] = 2;
}

void constructor_overload1_Bad() {
  std::vector<int> v(1);
  v[3] = 2;
}

void constructor_overload2_Good() {
  std::vector<int> v = {1};
  v[0] = 2;
}

void constructor_overload2_Bad() {
  std::vector<int> v = {1, 2, 3};
  v[v[2]] = 2;
}

void push_back_Good() {
  std::vector<int> v;
  v.push_back(1);
  v[0] = 2;
}

void push_back_Bad() {
  std::vector<int> v;
  v.push_back(1);
  v[1] = 2;
}

void emplace_back_Good() {
  std::vector<int> v;
  v.emplace_back(1);
  v[0] = 2;
}

void emplace_back_Bad() {
  std::vector<int> v;
  v.emplace_back(1);
  v[1] = 2;
}

void insert_overload1_Good() {
  std::vector<int> v;
  v.insert(v.begin(), 1);
  v[0] = 2;
}

void insert_overload1_Bad() {
  std::vector<int> v;
  v.insert(v.begin(), 1);
  v[1] = 2;
}

void insert_overload2_Good() {
  std::vector<int> v;
  v.insert(v.begin(), 10, 1);
  v[9] = 2;
}

void insert_overload2_Bad() {
  std::vector<int> v;
  v.insert(v.begin(), 10, 1);
  v[10] = 2;
}

void insert_overload3_Good() {
  std::vector<int> v;
  v.insert(v.begin(), {0, 1, 2});
  v[v[0]] = 2;
}

void insert_overload3_Bad() {
  std::vector<int> v;
  v.insert(v.begin(), {1, 2, 3});
  v[v[2]] = 2;
}

void reserve_Good() {
  std::vector<int> v;
  v.reserve(42);
  v.push_back(1);
  v[0] = 2;
}

void reserve_Bad() {
  std::vector<int> v;
  v.reserve(42);
  v[0] = 2;
}

void safe_access(std::vector<int> v) {
  if (v.size() >= 10) {
    v[9] = 1;
  }
}

void call_safe_access_Good_FP() {
  std::vector<int> v(5, 0);
  safe_access(v);
}

void safe_access2(std::vector<int> v) {
  if (v.empty()) {
    return;
  }

  unsigned int a[v.size()];
  for (unsigned int i = 0; i < v.size(); i++) {
    a[i] = 0;
  }
}

void call_safe_access2_Good() {
  std::vector<int> v;
  safe_access2(v);
}

void safe_access3_Good() {
  std::vector<int> v;
  if (!v.empty()) {
    v[0] = 1;
  }
}

void safe_access4(std::vector<int> v) {
  if (!v.empty()) {
    v[0] = 1;
  }
}

void call_safe_access4_Good() {
  std::vector<int> v;
  safe_access4(v);
}

void safe_access5(std::vector<int> v) {
  if (v.empty()) {
  } else {
    v[0] = 1;
  }
}

void call_safe_access5_Good() {
  std::vector<int> v;
  safe_access5(v);
}

void safe_access6(std::vector<int> v) {
  std::vector<int> v2(2);
  v2[v.empty()];
}

void call_safe_access6_Good() {
  std::vector<int> v;
  safe_access6(v);
}

void data_Good() {
  std::vector<int> v(5);
  int* p = v.data();
  p[4] = 1;
}

void data_Bad() {
  std::vector<int> v(5);
  int* p = v.data();
  p[4] = 10;
  p[v[4]] = 1;
}

void assert_Good() {
  std::vector<int> v;
  for (int i = 0; i < 5; i++) {
    v.push_back(1);
  }
  assert(v.size() == 5);
  v[4] = 1;
}

void assert_Good_2(int x) {
  std::vector<int> v;
  for (int i = 0; i < 5; i++) {
    v.push_back(1);
  }
  assert(((v.size() == 5) ? 1 : 0) ? 1 : 0);
  v[4] = 1;
}

void assert_Bad() {
  std::vector<int> v;
  for (int i = 0; i < 5; i++) {
    v.push_back(1);
  }
  assert(v.size() == 5);
  v[6] = 1;
}

class CharVector {
 public:
  CharVector(char* init) : a(init) {}

  char& operator[](int idx) { return a[idx]; }

 private:
  char* a;
};

void access_in_sixth(int count, CharVector v) {
  assert(count >= 0);
  assert(count < 5);
  v[count + 1] = '0';
}

void access_minus_one(int count, CharVector v) {
  assert(count >= 0);
  v[count - 1] = '0';
}

void precise_subst_Good() {
  char a[10];
  CharVector v(a);
  access_in_sixth(0, v);
}

void precise_subst_Good_FP() {
  char a[10];
  CharVector v(a);
  access_minus_one(1, v);
}

void precise_subst_Bad() {
  char a[10];
  CharVector v(a);
  access_minus_one(0, v);
}

void resize_Good() {
  std::vector<int> v;
  v.resize(1);
  v[0] = 0;
}

void resize_Bad() {
  std::vector<int> v;
  v.resize(1);
  v[1] = 0;
}

void iterate_rvalue_ref(std::vector<int>&& v) {
  [&]() {
    /* In SIL, the type iterator becomes a pointer to another pointer, i.e. an
       rvalue reference to vector. Inferbo should not raise an internal error in
       that case. */
    for (std::vector<int>::iterator it = v.begin(); it != v.end(); it++) {
    }
  };
}
