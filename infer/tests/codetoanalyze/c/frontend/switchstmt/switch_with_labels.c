/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int label_default(char x) {
  int ret = 0;
  switch (x) {
    case 1:
      ret++;
      goto l;
    case 2:
      ret = 2;
      break;
    l:
    default:
      ret--;
  }
  return ret;
}

int label_case(char x) {
  int ret = 0;
  switch (x) {
    case 1:
      ret++;
      goto l;
    l:
    case 2:
    case 3:
      ret++;
      break;
  }
  return ret;
}
