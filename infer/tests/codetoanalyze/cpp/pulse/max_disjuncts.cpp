/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <vector>

namespace max_disjuncts {

int rand_int();

// This returns the state with current maximum disjuncts (20).
int get_full_disjs() {
  switch (rand_int()) {
    case 0:
      return 0;
    case 1:
      return 1;
    case 2:
      return 2;
    case 3:
      return 3;
    case 4:
      return 4;
    case 5:
      return 5;
    case 6:
      return 6;
    case 7:
      return 7;
    case 8:
      return 8;
    case 9:
      return 9;
    case 10:
      return 10;
    case 11:
      return 11;
    case 12:
      return 12;
    case 13:
      return 13;
    case 14:
      return 14;
    case 15:
      return 15;
    case 16:
      return 16;
    case 17:
      return 17;
    case 18:
      return 18;
    default:
      return 19;
  }
}

bool unknown_bool();

void join_full_disjs1() {
  int x = get_full_disjs();
  if (unknown_bool()) {
    return; // 20 ContinueProgram disjuncts
  }
  throw "exc"; // 20 ExitProgram disjuncts
}

void call_join_full_disjs1_bad() {
  join_full_disjs1();
  int* p = nullptr;
  *p = 42;
}

void join_full_disjs2() {
  int x = get_full_disjs();
  if (unknown_bool()) {
    throw "exc"; // 20 ExitProgram disjuncts
  }
  return; // 20 ContinueProgram disjuncts
}

void call_join_full_disjs2_bad() {
  join_full_disjs2();
  int* p = nullptr;
  *p = 42;
}

void join_full_disjs3() {
  int x = get_full_disjs();
  bool b;
  if (unknown_bool()) {
    b = 1; // 20 ContinueProgram disjuncts with b = 1
  } else {
    b = 0; // 20 ContinueProgram disjuncts with b = 0
  }
  if (b) {
    throw "exc";
  }
  return;
}

void call_join_full_disjs3_bad() {
  join_full_disjs3();
  int* p = nullptr;
  *p = 42;
}

struct Arr {
  int arr[2];
  std::vector<int> vec;
};

void copy_and_modify_ok(const std::vector<int>& v, const Arr& arg) {
  int x = get_full_disjs(); // make max disjuncts
  auto copied_arg = arg; // copy arg
  for (const int& e : v) {
    copied_arg.arr[0] = 42; // modify copied_arg
    int y = arg.arr[0];
  }
}

void full_disjs_call_after_branch_bad_FN() {
  bool b = unknown_bool() ? 1 : 0;
  // two disjuncts
  // #0: {b=1}
  // #1: {b=0}

  get_full_disjs();
  // 20 disjuncts only with #0, i.e. in all disjuncts b=1.

  if (!b) {
    // 0 disjunct here!
    int* p = nullptr;
    *p = 42;
  }
}

class FullDisjsInLoop {
  Arr arr;

  void full_disjs_in_loop_ok1(int k) {
    Arr x;
    for (int i = 0; i < k; i++) {
      arr = x;
    }
  }

  void full_disjs_in_loop_ok2_FP(int k) {
    Arr x;
    get_full_disjs();
    for (int i = 0; i < k; i++) {
      // Pulse does not know `x` is copied multiple times here.
      arr = x;
    }
  }
};

} // namespace max_disjuncts
