/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// test templated globals manipulations

template <typename T>
bool templated_global;

void set_templated_global_int(bool b) { templated_global<int> = b; }

void set_templated_global_bool(bool b) { templated_global<bool> = b; }

// check that we handle different instantiations of the same templated global
// correctly
void several_instantiations_bad() {
  set_templated_global_int(true);
  set_templated_global_bool(false);
  if (templated_global<int> && !templated_global<bool>) {
    int* p = nullptr;
    *p = 42;
  }
}

// test global initializers

constexpr bool yes = true;

template <typename T>
constexpr bool templated_const_global = false;

template <>
constexpr bool templated_const_global<int> = true;

struct X;

void read_templated_const_global_then_crash_bad() {
  if (yes && templated_const_global<int> && !templated_const_global<X>) {
    int* p = nullptr;
    *p = 42;
  }
}
