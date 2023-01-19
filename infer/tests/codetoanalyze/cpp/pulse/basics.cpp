/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

int multiple_invalidations_branch_latent(int n, int* ptr) {
  if (n == 7) {
    delete ptr;
  } else {
    delete ptr;
  }
  return *ptr;
}

int multiple_invalidations_loop_latent(int n, int* ptr) {
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

struct A {
  ~A();
  int f(int i) const;
};

const A getA();

int struct_inside_loop_ok(std::vector<int> numbers) {
  int sum = 0;
  for (auto number : numbers) {
    A a = getA();
    sum += a.f(number);
  }
  return sum;
}

int struct_inside_loop_break_ok(std::vector<int> numbers) {
  int sum = 0;
  for (auto number : numbers) {
    A a = getA();
    if (number < 0) {
      break;
    }
    sum += a.f(number);
  }
  return sum;
}

int struct_inside_loop_continue_ok(std::vector<int> numbers) {
  int sum = 0;
  for (auto number : numbers) {
    A a = getA();
    if (number < 0) {
      continue;
    }
    sum += a.f(number);
  }
  return sum;
}

void return_from_inner_scope_ok(bool b) {
  {
    A a = getA();
    if (b) {
      return;
    }
  }
}

void return_inside_single_branch_if_in_loop_ok() {
  while (true) {
    if (true) {
      A a;
      return;
    }
  }
}

struct UseAfterSelfDestruct {
  A a_;
  int x_;
  ~UseAfterSelfDestruct() {
    if (x_ == 0) {
      a_ = getA();
    }
  }

  void reset_ok() {
    this->~UseAfterSelfDestruct();
    x_ = a_.f(x_);
  }
};

void call_builtin_add_overflow_ok() {
  int res;
  __builtin_add_overflow(2, 3, &res);
  if (res == 42) {
    int* q = nullptr;
    *q = 42;
  }
}

void call_builtin_add_overflow_bad() {
  int res;
  __builtin_add_overflow(2, 3, &res);
  if (res == 5) {
    int* q = nullptr;
    *q = 42;
  }
}

void call_builtin_mul_overflow_ok() {
  int res;
  __builtin_mul_overflow(2, 3, &res);
  if (res == 42) {
    int* q = nullptr;
    *q = 42;
  }
}

void call_builtin_mul_overflow_bad() {
  int res;
  __builtin_mul_overflow(2, 3, &res);
  if (res == 6) {
    int* q = nullptr;
    *q = 42;
  }
}

void call_builtin_sub_overflow_ok() {
  int res;
  __builtin_sub_overflow(3, 2, &res);
  if (res == 42) {
    int* q = nullptr;
    *q = 42;
  }
}

void call_builtin_sub_overflow_bad() {
  int res;
  __builtin_sub_overflow(3, 2, &res);
  if (res == 1) {
    int* q = nullptr;
    *q = 42;
  }
}
