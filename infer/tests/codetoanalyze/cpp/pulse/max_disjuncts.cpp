/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
