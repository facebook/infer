/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
