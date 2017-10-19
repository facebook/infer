/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
}
