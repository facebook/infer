/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

void construct_with_ptr_to_member() { List<item> l(&item::next); }

void noskip(List<item> l) {
  item i;
  l.add(&i);
  l.add_byref(i);
}

void assign_parenthesized_ptr_to_member(item& i, int item::*pm) {
  (i.*pm) = 0;
}

int read_ptr_to_member(item* i, int item::*pm) { return i->*pm; }

struct Handler {};

int call_ptr_to_member_function(Handler* h, int (Handler::*f)(int)) {
  return (h->*f)(42);
}

item call_ptr_to_member_function_returning_struct(Handler& h,
                                                  item (Handler::*f)()) {
  return (h.*f)();
}

struct WithUnion {
  union {
    int i;
    float f;
  };
};

void ptr_to_anonymous_union_member() { int WithUnion::*pm = &WithUnion::i; }
