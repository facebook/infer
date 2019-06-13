/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <chrono>
#include <mutex>

void alarm1_FN(std::timed_mutex& m) {
  m.lock();
  m.lock();
}

void try_lock_bad_FN(std::timed_mutex& m) {
  m.try_lock();
  m.lock();
}

void try_lock_for_bad_FN(std::timed_mutex& m) {
  m.try_lock_for(std::chrono::seconds(123));
  m.lock();
}

void try_lock_until_bad_FN(std::timed_mutex& m) {
  std::chrono::time_point<std::chrono::steady_clock> timeout;
  m.try_lock_until(timeout);
  m.lock();
}
