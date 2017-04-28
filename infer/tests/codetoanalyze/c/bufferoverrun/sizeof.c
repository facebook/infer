/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void eval_sizeof_bad() {
  if (sizeof(long long) < 10000) {
    // always true
    int a[0];
    a[1]; // report
  }
}
