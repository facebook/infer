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
  int x;
  line[0] = line[1];
  x = 0;
}

struct A {
  void locals_ok(int b) {
    if (b) {
      f();
    } else {
      mutex_.lock();
      f();
      mutex_.unlock();
    }
  }

  std::mutex mutex_;
};

} // namespace locals_char_array
