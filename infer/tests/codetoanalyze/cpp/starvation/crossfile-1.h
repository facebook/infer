/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef CROSS_FILE_ONE
#define CROSS_FILE_ONE

/* The goal of this test is to demonstrate that the per-file type
environments don't prevent the deadlock report here */

// unused include to distinguish the two per-file type environments
#include <vector>
#include <mutex>

class CrossFileOne;

#include "crossfile-2.h"

class CrossFileOne {
 public:
  CrossFileOne() {}
  void lock_my_mutex_first_then_the_other(CrossFileTwo* other);
  void just_lock_my_mutex();

 private:
  std::mutex _mutex;
};

#endif
