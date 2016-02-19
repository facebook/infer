/*
 * Copyright (c) 2013 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int main() {
  int k = 0;
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++) {
      k = k + i;
    }
  }
  return k;
}
