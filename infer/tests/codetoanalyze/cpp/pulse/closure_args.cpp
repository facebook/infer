/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

template <typename F>
int read_int_ref(const int& x, F f) {
  return x + x + x;
}

int temporary_and_closure_args_bad() {
  int v = read_int_ref(0, [](int a) { return 1; });
  if (v == 0) {
    int* p = nullptr;
    return *p;
  }
  return v;
}
