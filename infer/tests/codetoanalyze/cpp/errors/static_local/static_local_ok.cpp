/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
int* static_local_ok() {
  static int x = 1;
  return &x;
}

int static_local_caller() {
  int* p = static_local_ok();
  return *p;
}
