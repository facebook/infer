/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace UniqueLock {

int constructor0_ok() {
  std::mutex m;
  std::unique_lock<std::mutex> lock(m);
  auto raw = reinterpret_cast<int*>(lock.mutex());
  return *raw;
}

int constructor1_ok() {
  std::mutex m;
  std::defer_lock_t t;
  std::unique_lock<std::mutex> lock(m, t);
  auto raw = reinterpret_cast<int*>(lock.mutex());
  return *raw;
}

int constructor_bad() {
  std::unique_lock<std::mutex> lock;
  auto raw = reinterpret_cast<int*>(lock.mutex());
  return *raw;
}

int lock_ok() {
  std::mutex m;
  std::unique_lock<std::mutex> lock(m);
  lock.lock();
  return 0;
}

int copy_null_deref_bad() {
  std::unique_lock<std::mutex> l1;
  std::unique_lock<std::mutex> l2 = std::move(l1);
  auto raw = reinterpret_cast<int*>(l2.mutex());
  return *raw;
}

int assign_null_deref_bad() {
  std::mutex m;
  std::unique_lock<std::mutex> l1(m);
  std::unique_lock<std::mutex> l2;
  l1 = std::move(l2);
  auto raw = reinterpret_cast<int*>(l1.mutex());
  return *raw;
}

int move_deref_ok() {
  std::mutex m;
  std::unique_lock<std::mutex> l1(m);
  std::unique_lock<std::mutex> l2 = std::move(l1);
  auto raw = reinterpret_cast<int*>(l2.mutex());
  return *raw;
}

int assign_deref_ok() {
  std::mutex m;
  std::unique_lock<std::mutex> l1(m);
  std::unique_lock<std::mutex> l2 = std::move(l1);
  l1.release();
  auto raw = reinterpret_cast<int*>(l2.mutex());
  return *raw;
}

int move_null_deref_bad() {
  std::mutex m;
  std::unique_lock<std::mutex> l1(m);
  std::unique_lock<std::mutex> l2 = std::move(l1);
  auto raw = reinterpret_cast<int*>(l1.mutex());
  return *raw;
}

int self_move_assignment_ok() {
  std::mutex m;
  std::unique_lock<std::mutex> lock(m);
  lock = std::move(lock);
  auto raw = reinterpret_cast<int*>(lock.mutex());
  return *raw;
}

int release_ok() {
  std::mutex m;
  std::unique_lock<std::mutex> lock(m);
  auto raw = reinterpret_cast<int*>(lock.mutex());
  lock.release();
  return *raw;
}

int release_bad() {
  std::mutex m;
  std::unique_lock<std::mutex> lock(m);
  auto m1 = lock.mutex();
  auto m2 = lock.release();
  if (m1 == m2) {
    auto raw = reinterpret_cast<int*>(lock.mutex());
    return *raw;
  }
  return 0;
}

int swap_ok() {
  std::mutex m1;
  std::mutex m2;
  std::unique_lock<std::mutex> lock1(m1);
  std::unique_lock<std::mutex> lock2(m2);
  lock1.swap(lock2);
  if (&m1 != lock2.mutex() || &m2 != lock1.mutex()) {
    lock1.release();
    auto raw = reinterpret_cast<int*>(lock1.mutex());
    return *raw;
  }
  return 0;
}

int swap_bad() {
  std::mutex m1;
  std::mutex m2;
  std::unique_lock<std::mutex> lock1(m1);
  std::unique_lock<std::mutex> lock2(m2);
  lock1.swap(lock2);
  if (&m1 == lock2.mutex() && &m2 == lock1.mutex()) {
    lock1.release();
    auto raw = reinterpret_cast<int*>(lock1.mutex());
    return *raw;
  }
  return 0;
}

} // namespace UniqueLock
