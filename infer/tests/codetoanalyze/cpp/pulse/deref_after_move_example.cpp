/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <memory>

namespace deref_after_move_example {

struct Person {
  // NOTE: the implicit constructor has a mem leak FP
  std::unique_ptr<int> age{new int(35)};
  std::unique_ptr<int> move_age() { return std::move(age); }
  int access_age() { return *age; }
};

int deref_after_move_bad() {
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

struct SimpleStruct {
  int field;
};

std::unique_ptr<SimpleStruct> global_simple_struct;

void register_to_global(std::unique_ptr<SimpleStruct> x) {
  global_simple_struct = std::move(x);
}

void deref_after_register_ok(std::unique_ptr<SimpleStruct> x) {
  SimpleStruct* p = x.get();
  register_to_global(std::move(x));
  int n = p->field;
}

} // namespace deref_after_move_example
