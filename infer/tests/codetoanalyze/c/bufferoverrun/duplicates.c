/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void two_accesses(int* arr) {
  if (arr[1] < 0) {
    arr[1] = 0;
  }
}

void one_alarm_is_enough() {
  int arr[1];
  two_accesses(arr);
}
