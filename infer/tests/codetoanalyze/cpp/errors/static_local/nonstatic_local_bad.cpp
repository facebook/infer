/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int* nonstatic_local_bad() {
  int x = 1;
  return &x;
}

int nonstatic_local_caller() {
  int* p = nonstatic_local_bad();
  return *p;
}
