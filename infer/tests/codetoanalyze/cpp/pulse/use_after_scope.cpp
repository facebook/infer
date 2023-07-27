/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stddef.h>
#include <string>
#include <utility>

void invalidate_local_ok(int** pp) {
  int t = 0xdeadbeef;
  *pp = &t; // <-- potential bug here since t goes out of scope
}

void access_out_of_scope_stack_ref_bad() {
  int* p = NULL;
  invalidate_local_ok(&p);
  int k = *p; // accessing invalid
}

void no_access_out_of_scope_stack_ref_ok() {
  int* p = NULL;
  invalidate_local_ok(&p);
  // p is not accessed, hence ok
}

std::pair<std::string, int> return_make_pair() {
  std::string s = "abc";
  return std::make_pair(s, 42);
}

std::string make_pair_first_ok() { return return_make_pair().first; }
