/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

int if_bad() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  if (!x) {
    block = ^{
      ptr = NULL;
    };
  } else {
    block = ^{
    };
  }
  block(); // Calling block assigned in if branch
  return *ptr;
}

int if_good() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  if (!x) {
    block = ^{
    };
  } else {
    block = ^{
      ptr = NULL;
    };
  }
  block(); // Calling block assigned in if branch
  return *ptr;
}

int else_bad() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  if (x) {
    block = ^{
    };
  } else {
    block = ^{
      ptr = NULL;
    };
  }
  block(); // Calling block assigned in else branch
  return *ptr;
}

int else_good() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  if (x) {
    block = ^{
      ptr = NULL;
    };
  } else {
    block = ^{
    };
  }
  block(); // Calling block assigned in else branch
  return *ptr;
}

int conditional_call(int x) {
  __block int* ptr = &x;
  void (^block)(void);
  if (!x) {
    block = ^{
      ptr = NULL;
    };
  } else {
    block = ^{
    };
  }
  block();
  return *ptr;
}

int conditional_call_bad() { return conditional_call(0); }

int conditional_call_good() { return conditional_call(1); }

void apply_block(void (^block)(void)) { block(); }

// Needs analysis-time specialization
int apply_block_specialized_bad_FN() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  if (!x) {
    block = ^{
      ptr = NULL;
    };
  } else {
    block = ^{
    };
  }
  apply_block(block); // Calling block assigned in if branch
  return *ptr;
}

int apply_block_specialized_good() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  if (!x) {
    block = ^{
    };
  } else {
    block = ^{
      ptr = NULL;
    };
  }
  apply_block(block); // Calling block assigned in if branch
  return *ptr;
}
