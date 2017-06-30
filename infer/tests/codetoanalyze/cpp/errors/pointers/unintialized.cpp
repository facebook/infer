/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
void initialized_no_dangling_ok() {
  int* p = new int(42);
  delete p;
}

void uninitialized_dangling_bad() {
  int* p;
  delete p;
}
