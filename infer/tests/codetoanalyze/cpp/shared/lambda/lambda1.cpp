/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

int init_capture1() {
  return [i = 0]() { return i; }();
}

int init_capture2() {
  int i = 0;
  return [a = i, b = 0, c = 3]() { return a + b + c; }();
}

class Capture {
  void capture_this_explicit() {
    auto lambda = [this]() { return this; };
  }

  void capture_star_this() {
    auto lambda = [*this]() {

    };
  }

  void capture_this_with_equal() {
    auto lambda = [=]() { return this; };
  }

  void capture_this_with_auto() {
    auto lambda = [&]() { return this; };
  }
};

struct SomeStruct {
  int f;
  ~SomeStruct();
};

int struct_capture() {
  SomeStruct x;
  SomeStruct y;
  auto f = [x, y]() { return x.f + y.f; };
  return f();
}
