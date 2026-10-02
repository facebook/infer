/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <functional>

namespace std_invoke {

struct Counter {
  int count;
  void increment() { count++; }
};

void invoke_member_fn_ptr_then_npe_bad(Counter& counter) {
  std::invoke(&Counter::increment, counter);
  int* p = nullptr;
  *p = 42;
}

void invoke_data_member_ptr_then_npe_bad(Counter& counter) {
  int count = std::invoke(&Counter::count, counter);
  (void)count;
  int* p = nullptr;
  *p = 42;
}

} // namespace std_invoke
