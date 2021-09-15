/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace deref_after_mode_example {

struct Person {
  std::unique_ptr<int> age{new int(35)};
  std::unique_ptr<int> move_age() { return std::move(age); }
  int access_age() { return *age; }
};

int FN_deref_after_move_bad() {
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
} // namespace deref_after_mode_example
