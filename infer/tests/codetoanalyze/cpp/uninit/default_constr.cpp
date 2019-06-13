/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <string>

struct InitializingStruct {
  size_t match1;
  size_t count1;

  InitializingStruct();
};

InitializingStruct::InitializingStruct() : match1(0), count1(0) {}

struct SemiInitializingStruct {
  size_t match1;
  size_t count1;

  SemiInitializingStruct();
};

SemiInitializingStruct::SemiInitializingStruct() : match1(0) {}

struct NonInitializingStruct {
  size_t match1;
  size_t count1;
};

void foo(size_t, size_t){};

void init_OK() {
  InitializingStruct s;

  foo(s.count1, s.match1);
}

void FN_init() {
  SemiInitializingStruct s;

  foo(s.count1, s.match1);
}

void init_bad() {
  NonInitializingStruct s;

  foo(s.count1, s.match1);
}
