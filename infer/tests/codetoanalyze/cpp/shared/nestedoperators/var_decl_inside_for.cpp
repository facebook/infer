/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

int simple_init() {
  int result = 0;
  for (int i = 0; int x = 2; i++) {
    result += x;
  }
}

int init_with_scoped_var() {
  int result = 0;
  for (int i = 10; int x = i; i--) {
    result += x;
  }
}
