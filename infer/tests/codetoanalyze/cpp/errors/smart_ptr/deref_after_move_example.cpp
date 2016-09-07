/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>

namespace deref_after_mode_example {

struct Person {
  std::unique_ptr<int> age{new int(35)};
  std::unique_ptr<int> move_age() { return std::move(age); }
  int access_age() { return *age; }
};

int deref_after_move_crash() {
  Person p;
  auto x = p.move_age();
  *x;
  return p.access_age();
}

int deref_ok() {
  Person p;
  return p.access_age();
}

int deref_after_move_ok() {
  Person p;
  auto x = p.move_age();
  return *x;
}
}
