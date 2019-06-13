/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// std::__throw_bad_alloc() is not a standarized function. It could be defined
// in many places, so we need to consider all possibilities.
#include <new>
#include <stdexcept>

static int* get_null() { return nullptr; }

static void do_nothing() {}

int throw_if_null_ok() {
  int* p = get_null();
  if (p == nullptr)
    std::__throw_bad_alloc();
  return *p;
}

int nothrow_if_null_bad() {
  int* p = get_null();
  if (p == nullptr)
    do_nothing();
  return *p;
}
