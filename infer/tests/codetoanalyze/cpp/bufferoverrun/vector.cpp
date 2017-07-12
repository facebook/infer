/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <vector>
#include <list>

void out_of_bound_Bad(std::vector<int> v) {
  unsigned int n = v.size();
  v[n] = 1;
}

void constructor_Good() {
  std::vector<int> v(1);
  v[0] = 2;
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

void call_safe_access_Good() {
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

void call_safe_access2_Good_FP() {
  std::vector<int> v;
  safe_access2(v);
}

struct Int_no_copy {
  Int_no_copy(Int_no_copy&) = delete;
  Int_no_copy(const Int_no_copy&) = delete;
  Int_no_copy(volatile Int_no_copy&) = delete;
  Int_no_copy(const volatile Int_no_copy&) = delete;
  Int_no_copy(int i) : _i(i) {}
  int get_int() { return _i; }

 private:
  int _i;
};

void just_test_model_FP(void) {
  std::vector<Int_no_copy> v;
  v.push_back(Int_no_copy(0));
  v.erase(v.cbegin());
  v.erase(v.cbegin(), v.cend());
  const Int_no_copy ci(1);
  const Int_no_copy& cr = ci;
  v.insert(v.cbegin(), cr);
  v.insert(v.cbegin(), std::move(Int_no_copy(2)));
  v.emplace(v.cbegin(), 3);
  v.insert(v.cbegin(), 42, cr);
  int x = v[0].get_int();
  std::list<int> l;
  std::vector<int> v2(l.begin(), l.end());
  v2.insert(v2.cbegin(), l.begin(), l.end());
  v2.assign(l.begin(), l.end());
  const std::vector<int> v3{1, 2, 3};
  int y = v3[0];
  int z = v3.at(1);
}
