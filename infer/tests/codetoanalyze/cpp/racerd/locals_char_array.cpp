/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
