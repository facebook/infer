/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace aggregate_initialization {

// unnamed bit-fields are not members and get no initializer

struct Options {
  long size;
  const char* name;
  bool flag;
  long : 0;
};

void unnamed_bitfield_empty_init_bad() {
  Options o = {};
  if (!o.flag) {
    int* p = nullptr;
    *p = 42;
  }
}

void unnamed_bitfield_empty_init_ok() {
  Options o = {};
  if (o.flag || o.name != nullptr) {
    int* p = nullptr;
    *p = 42;
  }
}

struct Base {
  int a;
};

struct DerivedWithUnnamedBitfield : Base {
  int b;
  unsigned : 4;
  int c;
  int d;
};

void base_unnamed_bitfield_values_bad() {
  DerivedWithUnnamedBitfield x = {{1}, 2, 3, 4};
  if (x.a == 1 && x.b == 2 && x.c == 3 && x.d == 4) {
    int* p = nullptr;
    *p = 42;
  }
}

void base_unnamed_bitfield_values_ok() {
  DerivedWithUnnamedBitfield x = {{1}, 2, 3, 4};
  if (x.a != 1 || x.b != 2 || x.c != 3 || x.d != 4) {
    int* p = nullptr;
    *p = 42;
  }
}

void base_unnamed_bitfield_empty_init_bad() {
  DerivedWithUnnamedBitfield x = {};
  if (x.b == 0) {
    int* p = nullptr;
    *p = 42;
  }
}

struct DefaultMemberUnnamedBitfield {
  int a = 5;
  unsigned : 4;
  int b;
};

void default_member_unnamed_bitfield_bad() {
  DefaultMemberUnnamedBitfield x = {};
  if (x.a == 5 && x.b == 0) {
    int* p = nullptr;
    *p = 42;
  }
}

void default_member_unnamed_bitfield_ok() {
  DefaultMemberUnnamedBitfield x = {};
  if (x.a != 5 || x.b != 0) {
    int* p = nullptr;
    *p = 42;
  }
}

} // namespace aggregate_initialization
