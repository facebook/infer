/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

extern char** lib();

void extern_bad() {
  int arr[10];
  char** p = lib();
  if (p != 0)
    arr[10] = 0;
  char* q = *p;
  if (q != 0)
    arr[20] = 0;
  int r = *q;
  if (r != 0)
    arr[30] = 0;
}
