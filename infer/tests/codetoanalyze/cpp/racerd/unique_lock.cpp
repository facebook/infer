/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace basics {

class UniqueLock {
 public:
  UniqueLock() {}

  int well_guarded1_ok(int b, int new_value) {
    if (b) {
      std::unique_lock<std::mutex> g(mutex_);
      well_guarded1 = new_value;
      return 0;
    } else {
      std::lock_guard<std::mutex> lock(mutex_);
      return well_guarded1;
    }
  }

  int well_guarded2_deferred_ok(int b, int new_value) {
    if (b) {
      std::unique_lock<std::mutex> g(mutex_, std::defer_lock);
      g.lock();
      well_guarded2 = new_value;
      g.unlock();
      return 0;
    } else {
      std::unique_lock<std::mutex> g(mutex_);
      return well_guarded2;
    }
  }

  int not_guarded1_ok(int b, int new_value) {
    if (b) {
      not_guarded1 = new_value;
      return 0;
    } else {
      return not_guarded1;
    }
  }

  int not_guarded2_ok(int b, int new_value) {
    if (b) {
      std::unique_lock<std::mutex> g(mutex_);
      g.unlock();
      not_guarded2 = new_value;
      return 0;
    } else {
      std::unique_lock<std::mutex> g(mutex_);
      g.unlock();
      return not_guarded2;
    }
  }

  int suspiciously_read1_bad(int b, int new_value) {
    if (b) {
      std::unique_lock<std::mutex> g(mutex_);
      suspiciously_read1 = new_value;
      return 0;
    } else {
      return suspiciously_read1;
    }
  }

  int suspiciously_written_ok(int b, int new_value) {
    if (b) {
      suspiciously_written = new_value;
      return 0;
    } else {
      std::unique_lock<std::mutex> g(mutex_);
      return suspiciously_written;
    }
  }

  int suspiciously_read2_trylock_bad(int b, int new_value) {
    if (b) {
      std::unique_lock<std::mutex> g(mutex_);
      suspiciously_read2 = new_value;
      return 0;
    } else {
      std::unique_lock<std::mutex> lock(mutex_, std::try_to_lock);
      if (lock.owns_lock()) {
        return 0;
      } else {
        return suspiciously_read2;
      }
    }
  }

  int suspiciously_read3_deferlock_bad(int b, int new_value) {
    if (b) {
      std::unique_lock<std::mutex> g(mutex_);
      suspiciously_read3 = new_value;
      return 0;
    } else {
      std::unique_lock<std::mutex> lock(mutex_, std::defer_lock);
      if (lock.try_lock()) {
        return 0;
      } else {
        return suspiciously_read3;
      }
    }
  }

 private:
  int well_guarded1;
  int well_guarded2;
  int suspiciously_read1;
  int suspiciously_read2;
  int suspiciously_read3;
  int suspiciously_written;
  int not_guarded1;
  int not_guarded2;
  std::mutex mutex_;
};
} // namespace basics
