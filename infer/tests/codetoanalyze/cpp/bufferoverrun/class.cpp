/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
class my_class {
  int idx;
  int arr[10];

  void set_a(int n) { idx = n; }

  int id(int n) { return n; }

 public:
  int access_Bad() {
    set_a(10);
    return arr[idx];
  }

  int access2_Bad() {
    int n = 10;
    return arr[id(n)];
  }
};
