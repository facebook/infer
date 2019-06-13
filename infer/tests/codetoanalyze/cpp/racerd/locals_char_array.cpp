/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <mutex>

namespace locals_char_array {

void f() {
  char line[1024];
  line[0] = line[1];
}

struct A {
  void not_locked() { f(); }

  bool locked() {
    mutex_.lock();
    f();
  }
  std::mutex mutex_;
};

} // namespace locals_char_array
