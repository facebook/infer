/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <vector>

namespace havoc_loops {

void npe_after_push_back_loop_bad() {
  std::vector<int> v;
  for (int i = 0; i < 8; i++) {
    v.push_back(i);
  }
  int* p = nullptr;
  *p = 42;
}

struct Holder {
  int* f = nullptr;
  int n = 0;
  int size() const { return n; }
  void set(int* v) { f = v; }
};

void npe_after_loop_calling_const_method_bad() {
  Holder h;
  for (int i = 0; i < 10; i++) {
    h.size();
  }
  *h.f = 42;
}

void late_write_by_method_in_loop_ok() {
  int x = 0;
  Holder h;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      h.set(&x);
    }
  }
  *h.f = 42;
}

template <typename F>
void call(F f) {
  f();
}

void late_write_by_lambda_in_loop_ok() {
  int x = 0;
  int* p = nullptr;
  for (int i = 0; i < 10; i++) {
    if (i == 7) {
      call([&]() { p = &x; });
    }
  }
  *p = 42;
}

struct Entry {
  int* p = nullptr;
};

void late_write_through_reference_in_loop_ok(Entry* table) {
  int x = 0;
  table[7].p = nullptr;
  for (int i = 0; i < 16; i++) {
    Entry& e = table[i];
    e.p = &x;
  }
  *table[7].p = 42;
}

struct Table {
  int* slots[16] = {};
  void add(int* p) {
    for (int i = 0; i < 16; i++) {
      if (i == 15) {
        slots[i] = p;
      }
    }
  }
};

void store_in_loop_of_callee_no_leak_ok() {
  Table t;
  int* p = new int(0);
  t.add(p);
  delete t.slots[15];
}

struct Bounds {
  std::vector<int> largest;
};

// the copy is needed since its source is deleted before it is used; this is
// also reported without the option when no loop precedes the copy
void FP_copy_from_deleted_source_after_loop_ok(Bounds* b,
                                               std::vector<int>* out,
                                               int* a) {
  for (int i = 0; i < 16; i++) {
    a[i] = 0;
  }
  std::vector<int> end;
  end = b->largest;
  delete b;
  *out = end;
}

} // namespace havoc_loops
