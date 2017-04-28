/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
int const_in_param1(int const* p) { return *p; }

int const_in_param2(int const* const p) { return *p; }

int call_const_params_with_address() {
  int x = 1;
  const_in_param1(&x);
  const_in_param2(&x);

  const int cx = 0;

  const_in_param1(&cx);
  const_in_param2(&cx);
}

int call_const_params_with_pointer1() {
  int* p = nullptr;
  const_in_param1(p);
}
int call_const_params_with_pointer2() {
  int* p = nullptr;
  const_in_param2(p);
}

int call_const_params_with_pointer3() {
  int* const cp = nullptr;
  const_in_param2(cp);
}
