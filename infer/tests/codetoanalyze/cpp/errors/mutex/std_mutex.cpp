/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <mutex>

void alarm1_FN(std::mutex& m) {
  m.lock();
  m.lock();
}

void alarm2_FN(std::mutex& m) {
  m.try_lock();
  m.lock();
}

void no_alarm1(std::mutex& m) {
  for (int i = 0; i < 999999; i++) {
    m.try_lock();
  }
}

void ensure_locked(std::mutex& m) { m.try_lock(); }

void ensure_unlocked(std::mutex& m) {
  ensure_locked(m);
  m.unlock();
}

void ends_locked(std::mutex& m) {
  for (int i = 0; i < 999999; i++) {
    ensure_unlocked(m);
    ensure_locked(m);
  }
}

void starts_with_lock(std::mutex& m) {
  for (int i = 0; i < 999999; i++) {
    m.lock();
    ensure_unlocked(m);
  }
}

void alarm3_FN() {
  std::mutex m;
  ends_locked(m);
  starts_with_lock(m);
}

void no_alarm3(bool b) {
  std::mutex m;
  if (b) {
    m.lock();
  }
  if (b) {
    m.unlock();
  }
}

void alarm3(bool b) {
  std::mutex m;
  if (b) {
    m.lock();
  }
  if (b) {
    m.lock();
  }
}
