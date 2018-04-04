/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#include <string>
#include <vector>

struct Aggregate {
  int i;

  ~Aggregate() {}
};

void aggregate_reassign_ok() {
  const int len = 5;
  Aggregate arr[len];
  for (int i = 0; i < len; i++) {
    Aggregate s = {1};
    // assign with curly bracket syntax doesn't call constructor; need to
    // recognize that this is a reassignment anyway
    arr[0] = s; // shouldn't be flagged as a use-after-lifetime
  }
}

struct AggregateWithConstructedField {
  std::string str;
};

void aggregate_reassign2_ok() {
  AggregateWithConstructedField arr[10];
  for (int i = 0; i < 10; i++) {
    // this is translated as string(&(a.str), "hi"). need to make sure this is
    // treated the same as initializing a
    AggregateWithConstructedField a{"hi"};
    arr[i] = a;
  }
}

struct NestedAggregate {
  AggregateWithConstructedField a;
};

void aggregate_reassign3_ok() {
  NestedAggregate arr[10];
  for (int i = 0; i < 10; i++) {
    // this is translated as std::basic_string(&(a.str), "hi"). need to make
    // sure this is treated the same as initializing a
    NestedAggregate a{{"hi"}};
    arr[i] = a;
  }
}

int multiple_invalidations_branch_bad(int n, int* ptr) {
  if (n == 7) {
    delete ptr;
  } else {
    delete ptr;
  }
  return *ptr;
}

int multiple_invalidations_loop_bad(int n, int* ptr) {
  for (int i = 0; i < n; i++) {
    if (i == 7) {
      delete ptr;
    } else {
      delete ptr;
    }
  }
  return *ptr;
}

Aggregate* pointer_arithmetic_ok(Aggregate* a) {
  a->~Aggregate();
  a++;
  return a;
}

void iterator_pointer_arithmetic_ok(std::vector<Aggregate> v) {
  for (auto it = v.begin(); it != v.end(); ++it) {
    it->~Aggregate();
  }
}
