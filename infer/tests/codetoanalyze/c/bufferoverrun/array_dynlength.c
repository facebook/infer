/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

void init_variable_array(int len) {
  int x = 2 * len;
  int a[len + x + 1];
  a[len + x + 1] = 0;
}
