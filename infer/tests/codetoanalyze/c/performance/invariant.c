/*
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
/* We get quadratic bound for this example but we should get linear bound */
void do_k_times(int n) {
  int k = n;
  for (int i = 0; i < k; i++) {
  }
}
