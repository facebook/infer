/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace basics {

class BasicsNoMutex {
 public:
  BasicsNoMutex() {}

  void set(int new_value) {
    field_1 = new_value;
    field_2 = new_value;
    field_3 = new_value;
  }

  int get1() { return field_1; }

  int get2() { return field_2; }

 private:
  int field_1;
  int field_2;
  int field_3;
};
} // namespace basics
