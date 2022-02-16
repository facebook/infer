/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@class HiddenCaptured;
;

@interface HiddenCaptured : NSObject

@property int (^get_hiddenProperty)(void);
@property void (^set_hiddenProperty)(int);

@end

@implementation HiddenCaptured

- (instancetype)init {
  self = [super init];
  if (self != nil) {
    __block int hidden_property = 0;
    self.get_hiddenProperty = ^() {
      return hidden_property;
    };
    self.set_hiddenProperty = ^(int x) {
      hidden_property = x;
    };
  }
  return self;
}

@end

int test_hiddenCaptured_bad() {
  HiddenCaptured* a = [HiddenCaptured new];
  a.set_hiddenProperty(0);
  int hidden = a.get_hiddenProperty(); // hidden = 0
  a.set_hiddenProperty(hidden + 1);
  hidden = a.get_hiddenProperty(); // hidden = 1
  a.set_hiddenProperty(hidden + 1);
  hidden = a.get_hiddenProperty(); // hidden = 2
  a.set_hiddenProperty(hidden + 1);
  hidden = a.get_hiddenProperty(); // hidden = 3
  int* ptr = &hidden;
  if (hidden == 3) {
    ptr = NULL;
  }
  return *ptr; // Null deref here
}

typedef int (^block_type)(void);

block_type return_block() {
  __block int x = 0;
  block_type block = ^{
    return x;
  };
  return block;
}

int test_return_block_bad() {
  int returned = return_block()();
  int* ptr = &returned;
  if (!returned) {
    ptr = NULL;
  }
  return *ptr;
}
