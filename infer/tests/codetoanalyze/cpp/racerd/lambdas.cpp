/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

class Lambdas {
 public:
  void race_in_lambda_even_without_call_ok() {
    auto lambda_with_sync = [&]() {
      mutex_.lock();
      f = 0;
      mutex_.unlock();
      return f;
    };
  }

  // access propagation to callees does not currently work
  int FN_race_in_lambda_bad() {
    auto lambda_with_sync = [&]() { return g; };

    return lambda_with_sync();
  }

  void set_under_lock(int value) {
    mutex_.lock();
    g = value;
    mutex_.unlock();
  }

 private:
  int f;
  int g;
  std::mutex mutex_;
};
