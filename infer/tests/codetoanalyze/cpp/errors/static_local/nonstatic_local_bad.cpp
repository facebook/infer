/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
int* nonstatic_local_bad() {
  int x = 1;
  return &x;
}

int nonstatic_local_caller() {
  int* p = nonstatic_local_bad();
  return *p;
}
