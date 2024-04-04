/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

enum Enum {
  A = 0,
  B = 1,
};

class EnumWrapper {
  Enum v;

 public:
  EnumWrapper(Enum&& v) : v(v) {}
};

struct Super {};

struct Sub : Super {
 public:
  EnumWrapper f1;
  int f2;
};

Sub no_missing_node() { return Sub{.f1 = Enum::A}; }
