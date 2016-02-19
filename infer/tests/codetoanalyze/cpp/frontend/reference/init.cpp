/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void init_from_ref(int& par) {
  int v = par;
  int& d = par;
  int* p = &par;
}

void init_from_val(int par) {
  int v = par;
  int& d = par;
  int* p = &par;
}

void init_from_ptr(int* par) {
  int v = *par;
  int& d = *par;
  int* p = par;
}
