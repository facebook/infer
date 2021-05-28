/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/Foundation.h>

@interface Singleton : NSObject

@property int x;

@end

typedef void (^MyBlock)();

void dispatch(MyBlock block) { block(); }

@implementation Singleton

- (int)dispatch_once_no_npe_good {
  static Singleton* a = nil;
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    a = [[Singleton alloc] init];
  });
  return a->_x;
}

- (int)dispatch_once_captured_vars_bad {
  static dispatch_once_t onceToken;
  int* x = NULL;
  int val = 5;
  __block int* y = &val;
  dispatch_once(&onceToken, ^{
    y = x;
  });
  return *y;
}

- (int)dispatch_no_npe_good {
  static Singleton* a = nil;
  static dispatch_once_t onceToken;
  dispatch(^{
    a = [[Singleton alloc] init];
    a->_x = 5;
  });
  return a->_x;
}

@end

int captured_npe_bad() {
  int* x = NULL;
  int (^my_block)(void) = ^() {
    return *x;
  };
  return my_block();
}

int captured_npe_ok(int* y) {
  __block int* x = NULL;
  void (^my_block)(void) = ^() {
    x = y;
  };
  my_block();
  return *x;
}
