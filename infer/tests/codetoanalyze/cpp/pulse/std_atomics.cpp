/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <atomic>

namespace atomic_test {

void fetch_sub_add_ok() {
  std::atomic<int> a{0};
  int* p = nullptr;
  a.fetch_add(1, std::memory_order_acq_rel);
  a.fetch_add(1);
  a.fetch_sub(1, std::memory_order_acq_rel);
  a.fetch_sub(1);
  int result_zero = a.fetch_sub(1);
  if (result_zero != 0) {
    *p = 42;
  }
}

void pre_increment_decrement_test_ok() {
  std::atomic<int> a{0};
  int* p = nullptr;
  int x = ++a;
  int y = --a;
  if (a != 0 || x != 1 || y != a) {
    *p = 42;
  }
}

void pre_increment_decrement_test_bad() {
  std::atomic<int> a{0};
  int* p = nullptr;
  ++a;
  --a;
  if (a == 0) {
    *p = 42;
  }
}

void post_increment_decrement_test_ok() {
  std::atomic<int> a{0};
  int* p = nullptr;
  int x = a++;
  int y = a--;
  if (a != 0 || x != 0 || y != 1) {
    *p = 42;
  }
}

void load_store_impossible_npe_ok() {
  std::atomic<int> a(0);
  int* p = nullptr;
  a.store(1);
  if (a.load() != 1) {
    *p = 42;
  }
}

void load_store_possible_npe_bad() {
  std::atomic<int> a(0);
  int* p = nullptr;
  a.store(1);
  if (a.load() == 1) {
    *p = 42;
  }
}

void exchange_impossible_npe_ok() {
  std::atomic<int> a(0);
  int* p = nullptr;
  int b = a.exchange(1);
  if (a != 1 || b != 0) {
    *p = 42;
  }
}

void exchange_possible_npe_bad() {
  std::atomic<int> a(0);
  int* p = nullptr;
  int b = a.exchange(1);
  if (a == 1 && b == 0) {
    *p = 42;
  }
}

void FP_compare_exchange_weak_impossible_npe1_ok() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a != 2 || b != 0 || !succ) {
    *p = 42;
  }
}

void compare_exchange_weak_possible_npe1_bad() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a == 2 && b == 0 && succ) {
    *p = 42;
  }
}

void FP_compare_exchange_weak_impossible_npe2_ok() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a != 0 || b != 0 || succ) {
    *p = 42;
  }
}

void compare_exchange_weak_possible_npe2_bad() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_weak(b, 2);
  if (a == 0 && b == 0 && !succ) {
    *p = 42;
  }
}

void FP_compare_exchange_strong_impossible_npe1_ok() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a != 2 || b != 0 || !succ) {
    *p = 42;
  }
}

void compare_exchange_strong_possible_npe1_bad() {
  std::atomic<int> a(0);
  int b = 0;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a == 2 && b == 0 && succ) {
    *p = 42;
  }
}

void FP_compare_exchange_strong_impossible_npe2_ok() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a != 0 || b != 0 || succ) {
    *p = 42;
  }
}

void compare_exchange_strong_possible_npe2_bad() {
  std::atomic<int> a(0);
  int b = 1;
  int* p = nullptr;
  int succ = a.compare_exchange_strong(b, 2);
  if (a == 0 && b == 0 && !succ) {
    *p = 42;
  }
}
} // namespace atomic_test
