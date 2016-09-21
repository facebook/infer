/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

void using_value() {
  int v = 3;
  int& r = ++v;
  int& q = --v;
}

void using_ref() {
  int v = 3;
  int& vr = v;
  int& r = ++vr;
  int& q = --vr;
}
