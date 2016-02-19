/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

enum ENUM { val1, val2 };

template <class T>
T get() {
  return T();
}

void test() {
  int i = get<int>();
  float f = get<float>();
  float* fp = get<float*>();
  get<void>();
  ENUM x = get<ENUM>();
  float f2 = float();
}
