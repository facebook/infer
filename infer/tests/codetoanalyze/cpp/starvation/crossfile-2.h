/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* The goal of this test is to demonstrate that the per-file type
environments don't prevent the deadlock report here */

#ifndef CROSS_FILE_TWO
#define CROSS_FILE_TWO

#include <mutex>

class CrossFileTwo;

#include "crossfile-1.h"

class CrossFileTwo {
 public:
  CrossFileTwo() {}
  void lock_my_mutex_first_then_the_other(CrossFileOne* other);
  void just_lock_my_mutex();

 private:
  std::mutex _mutex;
};

#endif
