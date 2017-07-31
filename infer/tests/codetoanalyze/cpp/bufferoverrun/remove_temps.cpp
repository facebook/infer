/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <vector>

struct S {
  std::vector<int> l;
  S* next;
  std::atomic<void*> ptr;
};

class C {
  void foo_Bad();
  void goo();
  S head_;
};

void C::foo_Bad() {
  while (1) {
    S* t = head_.next;
    int x = t->l.size();
    void* ptr = t->ptr;
    int a[5];
    a[10] = 1;
  }
}

void C::goo() {
  while (1) {
    S* t = head_.next;
    int x = t->l.size();
    void* ptr = t->ptr;
  }
}
