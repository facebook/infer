/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
