/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <mutex>
std::mutex mtx1;
std::mutex mtx2;

void mtx1_mtx2() {
  std::unique_lock<std::mutex> g1(mtx1);
  std::unique_lock<std::mutex> g2(mtx2);
}

std::unique_lock<std::mutex> lock_with_unique() {
  return std::unique_lock<std::mutex>(mtx2);
}

void unlock_lck(std::unique_lock<std::mutex>& l) { l.unlock(); }

// This test is meant to demostrate that starvation
// doesn't work with guards. It's not supposed to deadlock
// becuase guard is unlocked with unlock_lck(g1), but guards
// aren't tracked interprocredurally, so this doesn't work.
// For now, we just disabled guarded locks in interprocedural setting
// to fix this issue

void mtx2_mtx1() {
  auto g1 = lock_with_unique();
  unlock_lck(g1);
  std::unique_lock<std::mutex> g2(mtx1);
  // Need to unlock here, because otherwise mtx2 is unbalanced and
  //  it would be filtered due to that
  mtx2.unlock();
}
