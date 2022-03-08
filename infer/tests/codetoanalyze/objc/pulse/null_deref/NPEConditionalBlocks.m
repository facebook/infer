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

int apply_block_specialized_bad() {
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

void apply_block_and_after(void (^block)(void), void (^after)(void)) {
  int x = 0;
  if (x) {
    block = ^{
    };
  }
  apply_block(block);
  after();
}

int apply_block_and_after_specialized_bad() {
  int x = 0;
  __block int* ptr = &x;
  void (^block)(void);
  void (^after)(void);
  if (!x) {
    block = ^{
      ptr = NULL;
    };
    after = ^{
      int x = *ptr;
    };
  } else {
    block = ^{
    };
    after = ^{
    };
  }
  apply_block_and_after(
      block,
      after); // Calling block assigned in if branch. NPE when calling after
  return *ptr;
}

int apply_block_and_after_respecialized_bad() {
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
  apply_block_and_after(block, ^{
    int x = *ptr; // NPE here
  }); // Calling block assigned in if branch
  return *ptr;
}

void conditionnaly_apply_block(int x, int** ptr, void (^block)(void)) {
  if (x) {
    block();
  }
  *ptr = NULL;
}

int conditionnaly_apply_block_unspecialized_bad() {
  int x = 0;
  __block int* ptr = NULL;
  void (^block)(void);
  if (!x) {
    block = ^{
    };
  } else {
    block = ^{
    };
  }
  conditionnaly_apply_block(
      x, &ptr, block); // block is not called; function is not specialized
  return *ptr;
}

int conditionnaly_apply_block_specialized_bad() {
  int x = 1;
  __block int* ptr = NULL;
  void (^block)(void);
  if (!x) {
    block = ^{
    };
  } else {
    block = ^{
    };
  }
  conditionnaly_apply_block(
      x, &ptr, block); // Calling block assigned in else branch
  return *ptr;
}
