/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace globals {

std::mutex mutex_1;
std::mutex mutex_2;
int x;

// deadlock between lock_1_then_2_bad() and lock_2_then_1_bad(), reported once
void lock_1_then_2_bad() {
  std::lock_guard<std::mutex> lock1(mutex_1);
  std::lock_guard<std::mutex> lock2(mutex_2);
  x++;
}

void lock_2_then_1_bad() {
  std::lock_guard<std::mutex> lock2(mutex_2);
  std::lock_guard<std::mutex> lock1(mutex_1);
  x--;
}

void lock_and_increment(std::mutex& mutex) {
  std::lock_guard<std::mutex> lock(mutex);
  x++;
}

std::mutex helper_mutex_1;
std::mutex helper_mutex_2;

// deadlock between helper_1_then_2_bad() and helper_2_then_1_bad(), whose
// second locks have the same location, reported once
void helper_1_then_2_bad() {
  std::lock_guard<std::mutex> lock1(helper_mutex_1);
  lock_and_increment(helper_mutex_2);
}

void helper_2_then_1_bad() {
  std::lock_guard<std::mutex> lock2(helper_mutex_2);
  lock_and_increment(helper_mutex_1);
}

} // namespace globals
