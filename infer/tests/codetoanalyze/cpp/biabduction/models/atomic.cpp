/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <atomic>

namespace atomic_test {

class A {
  std::atomic<int> x;

 public:
  A() : x(0) {}
  void add() { ++x; }
  void sub() { --x; }
  bool is_zero() const { return x == 0; }
};

void FP_is_zero_impossible_npe_ok() {
  A a;
  int* p = nullptr;
  a.add();
  a.sub();
  if (!a.is_zero())
    *p = 42;
}

void is_zero_possible_npe_bad() {
  A a;
  int* p = nullptr;
  a.add();
  a.sub();
  if (a.is_zero())
    *p = 42;
}

void FP_not_zero_impossible_npe_ok() {
  A a;
  int* p = nullptr;
  a.add();
  a.add();
  if (a.is_zero())
    *p = 42;
}

void not_zero_possible_npe_bad() {
  A a;
  int* p = nullptr;
  a.sub();
  a.sub();
  if (!a.is_zero())
    *p = 42;
}

void FP_load_store_impossible_npe_ok() {
  std::atomic<int> a(0);
  int* p = nullptr;
  a.store(1);
  if (a.load() != 1)
    *p = 42;
}

void load_store_possible_npe_bad() {
  std::atomic<int> a(0);
  int* p = nullptr;
  a.store(1);
  if (a.load() == 1)
    *p = 42;
}

void FP_exchange_impossible_npe_ok() {
  std::atomic<int> a(0);
  int* p = nullptr;
  int b = a.exchange(1);
  if (a != 1 || b != 0)
    *p = 42;
}

void exchange_possible_npe_bad() {
  std::atomic<int> a(0);
  int* p = nullptr;
  int b = a.exchange(1);
  if (a == 1 && b == 0)
    *p = 42;
}

void FP_compare_exchange_weak_impossible_npe1_ok() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a != 2 || b != 0 || !succ)
    *p = 42;
}

void compare_exchange_weak_possible_npe1_bad() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a == 2 && b == 0 && succ)
    *p = 42;
}

void FP_compare_exchange_weak_impossible_npe2_ok() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a != 0 || b != 0 || succ)
    *p = 42;
}

void compare_exchange_weak_possible_npe2_bad() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a == 0 && b == 0 && !succ)
    *p = 42;
}

void FP_compare_exchange_strong_impossible_npe1_ok() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a != 2 || b != 0 || !succ)
    *p = 42;
}

void compare_exchange_strong_possible_npe1_bad() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a == 2 && b == 0 && succ)
    *p = 42;
}

void FP_compare_exchange_strong_impossible_npe2_ok() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a != 0 || b != 0 || succ)
    *p = 42;
}

void compare_exchange_strong_possible_npe2_bad() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a == 0 && b == 0 && !succ)
    *p = 42;
}
} // namespace atomic_test
