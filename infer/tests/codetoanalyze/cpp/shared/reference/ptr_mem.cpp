/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
struct item {
  int data;
  item* next;
};

template <typename E>
struct List {
  List(E* E::*next_ptr) : head(nullptr), next_ptr(next_ptr) {}

  void add(E* e) {
    e->*next_ptr = head;
    head = e;
  }

  void add_byref(E& e) {
    e.*next_ptr = head;
    head = &e;
  }

  E* head;
  E* E::*next_ptr;
};

void skip() { List<item> l(&item::next); }

void noskip(List<item> l) {
  item i;
  l.add(&i);
  l.add_byref(i);
}
