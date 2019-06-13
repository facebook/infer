/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <mutex>

namespace detail {

inline int lock_impl(std::mutex* lock) {
  lock->lock();
  return 0;
}

inline int try_lock_impl(std::mutex* lock) {
  if (lock->try_lock()) {
    return 0;
  }
  return EBUSY;
}

} // namespace detail

template <typename T>
int lp_lock(T* lock) {
  int rv = detail::try_lock_impl(lock);

  switch (rv) {
    case 0:
      return 0;
    case EBUSY: {
      rv = detail::lock_impl(lock);
      if (rv == 0) {
        return 0;
      }
      /* fallthrough */
    }
    default:
      return rv;
  }
}

void good_usage() {
  std::mutex m;
  lp_lock(&m);
}

void bad_usage1_FN() {
  std::mutex m;
  lp_lock(&m);
  lp_lock(&m);
}

void bad_usage2_FN() {
  std::mutex m;
  m.lock();
  lp_lock(&m);
}

template <typename T>
class LpLockGuard {
 public:
  LpLockGuard(T& lock) : lock_(lock) { lp_lock(&lock_); }

 private:
  T& lock_;
};

struct LockMapBucket {
 public:
  void good_usage2() { LpLockGuard<std::mutex> lock(bucketLock); }

  void bad_usage3_FN() {
    LpLockGuard<std::mutex> lock(bucketLock);
    this->good_usage2();
  }

 private:
  std::mutex bucketLock; // Lock protecting against concurrent access
};
