/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
