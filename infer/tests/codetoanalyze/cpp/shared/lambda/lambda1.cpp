/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

int bar() {
  auto func = []() {
    int i = 0;
    return i;
  };
  return 7 / func();
}

int foo() {
  auto unused = []() { return 1 / 0; };
  auto y = [](int i) { return ++i; };
  return 5 / (4 - y(3));
}

int fooOK() {

  auto y = [](int i) { return i++; };
  return 5 / (4 - y(3));
}

int normal_capture() {
  int x = 1;
  int y = 2;
  return [x, y]() { return x + y; }();
}

int capture_by_ref() {
  int x = 0;
  [&x]() { x++; }();
  return x;
}

void init_capture() {
  [i = 0]() { i; };
}
