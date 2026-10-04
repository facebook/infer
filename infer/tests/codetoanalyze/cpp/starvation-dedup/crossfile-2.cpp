/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "crossfile.h"

namespace crossfile {

// deadlock with WithMutexes::method_1_then_2_bad()
void free_function_2_then_1_bad(WithMutexes* s) {
  std::lock_guard<std::mutex> lock2(s->mutex_2);
  std::lock_guard<std::mutex> lock1(s->mutex_1);
  s->x--;
}

// deadlock with MemberAndGlobal::member_then_global_bad()
void MemberAndGlobal::global_then_member_bad() {
  std::lock_guard<std::mutex> lock2(global_mutex);
  std::lock_guard<std::mutex> lock1(mutex_);
  x_--;
}

} // namespace crossfile
