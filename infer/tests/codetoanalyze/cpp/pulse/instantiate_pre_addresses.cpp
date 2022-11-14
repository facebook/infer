/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

struct rec {
  int* x;
};

void mutate_deep_pointers(rec** ptr) {
  (*ptr)->x = new int;
  (*(*ptr)->x) = 24;
  *ptr = new rec;
}

void no_leak_nullptr() {
  rec* r1 = new rec{nullptr};
  rec* r2 = r1;

  mutate_deep_pointers(&r1);

  if (*(r2->x) != 24) {
    int* x = nullptr;
    *x = 12;
  }
  *(r2->x) = 42;

  delete r1;
  delete r2->x;
  delete r2;
}

void set_inner_pointer_bad() {
  rec* r1 = new rec{nullptr};
  rec* r2 = r1;

  mutate_deep_pointers(&r1);

  if (*(r2->x) == 24) {
    int* x = nullptr;
    *x = 12;
  }

  delete r1;
  delete r2->x;
  delete r2;
}

struct list_item {
  list_item* next;
};

struct list {
  list_item* first;
  list_item* last;
};

void append(list& l) {
  list_item* new_cell = new list_item;
  new_cell->next = nullptr;

  if (l.last == nullptr) {
    l.first = new_cell;
  } else {
    l.last->next = new_cell;
  }

  l.last = new_cell;
}

void delete_list(list& l) {
  list_item* cell = l.first;
  list_item* next_cell;

  while (cell != nullptr) {
    next_cell = cell->next;
    delete cell;
    cell = next_cell;
  }
}

void no_leak() {
  list l = {nullptr, nullptr};

  append(l);
  append(l);

  delete_list(l);
}
