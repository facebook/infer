/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <mutex>

namespace crossfile {

extern std::mutex global_mutex;

struct WithMutexes {
  std::mutex mutex_1;
  std::mutex mutex_2;
  int x;

  void method_1_then_2_bad();
};

void free_function_2_then_1_bad(WithMutexes* s);

class MemberAndGlobal {
 public:
  void member_then_global_bad();
  void global_then_member_bad();

 private:
  std::mutex mutex_;
  int x_;
};

} // namespace crossfile
