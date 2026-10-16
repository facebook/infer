/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "crossfile.h"

namespace crossfile {

std::mutex global_mutex;

// does not see free_function_2_then_1_bad() in the other file, so the deadlock
// is reported there
void WithMutexes::method_1_then_2_bad() {
  std::lock_guard<std::mutex> lock1(mutex_1);
  std::lock_guard<std::mutex> lock2(mutex_2);
  x++;
}

// does not see global_then_member_bad() in the other file, so the deadlock is
// reported there
void MemberAndGlobal::member_then_global_bad() {
  std::lock_guard<std::mutex> lock1(mutex_);
  std::lock_guard<std::mutex> lock2(global_mutex);
  x_++;
}

} // namespace crossfile
