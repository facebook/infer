/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
