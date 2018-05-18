/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <memory>
#include <string>
#include <vector>

struct S {
  int f;
};

void deref_deleted_bad() {
  auto s = new S{1};
  delete s;
  S tmp = *s;
}

S* return_deleted_bad() {
  auto s = new S{1};
  delete s;
  return s;
}

S* reassign_deleted_ok() {
  auto s = new S{1};
  delete s;
  s = new S{2};
  return s;
}

void reassign_field_of_deleted_bad() {
  auto s = new S{1};
  delete s;
  s->f = 7;
}

void reassign_field_of_reinitialized_ok(S* tmp) {
  auto s = new S{1};
  delete s;
  s = tmp;
  s->f = 7;
}

void double_delete_bad() {
  auto s = new S{1};
  delete s;
  delete s;
}

S* delete_in_branch_bad(bool b) {
  auto s = new S{1};
  if (b) {
    delete s;
  }
  return s;
}

void delete_in_branch_ok(bool b) {
  auto s = new S{1};
  if (b) {
    delete s;
  } else {
    delete s;
  }
}

void use_in_branch_bad(bool b) {
  auto s = new S{1};
  delete s;
  if (b) {
    auto tmp = *s;
  }
}

void delete_in_loop_bad() {
  auto s = new S{1};
  for (int i = 0; i < 10; i++) {
    delete s;
  }
}

void delete_in_loop_ok() {
  for (int i = 0; i < 10; i++) {
    auto s = new S{1};
    delete s;
  }
}

void delete_ref_in_loop_ok(int j, std::vector<std::string> v) {
  int i = 0;
  for (int i = 0; i < 10; i++) {
    auto s = &v[i];
    delete s;
  }
}

void use_in_loop_bad() {
  auto s = new S{1};
  delete s;
  for (int i = 0; i < 10; i++) {
    s->f = i;
  }
}

S* gated_delete_abort_ok(bool b) {
  auto s = new S{1};
  if (b) {
    delete s;
    std::abort();
  }
  return s;
}

S* gated_exit_abort_ok(bool b) {
  auto s = new S{1};
  if (b) {
    delete s;
    exit(1);
  }
  return s;
}

S* gated_delete_throw_ok(bool b) {
  auto s = new S{1};
  if (b) {
    delete s;
    throw 5;
  }
  return s;
}
