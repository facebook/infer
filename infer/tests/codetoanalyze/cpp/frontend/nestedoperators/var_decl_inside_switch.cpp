/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int get(int a) {
  switch (int x = a) {
    case 0:
    case 1:
      return 0;
    case 2:
      return 1;
    default:
      return x;
  }
}
