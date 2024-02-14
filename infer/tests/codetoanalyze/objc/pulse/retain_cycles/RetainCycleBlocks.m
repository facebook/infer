/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@class RCBlock;

typedef void (^MyHandler)(RCBlock* name);

@interface RCBlockAA : NSObject

@property(nonatomic, strong) RCBlock* b;

@property(nonatomic, strong) RCBlockAA* child;

@end

typedef void (^MyAHandler)(RCBlockAA* name);

@interface RCBlock : NSObject {
  RCBlock* child;
}

@property(nonatomic, strong) RCBlockAA* a;

@property(nonatomic, strong) MyHandler handler;

@property(nonatomic, strong) MyAHandler a_handler;

@property(nonatomic, strong) RCBlock* child;

@end

@implementation RCBlockAA
@end

@implementation RCBlock

// This code can be executed and one can check that there's a cycle because
// "Dealloc" is not being printed.
- (void)dealloc {
  NSLog(@"Dealloc");
}

- (void)retain_self_in_block_retain_cycle_bad {
  self.handler = ^(RCBlock* b) {
    self->_child = b;
  };
}

- (void)retain_weak_self_in_block_no_retain_cycle {
  __weak typeof(self) weak_self = self;
  self.handler = ^(RCBlock* b) {
    __strong typeof(self) strong_self = weak_self;
    if (strong_self)
      strong_self->_child = b;
  };
}

@end

int test_retain_self_in_block_cycle_bad() {
  RCBlock* c = [[RCBlock alloc] init];
  [c retain_self_in_block_retain_cycle_bad];
  return 0;
}

int test_weak_self_in_block_no_retain_cycle_good() {
  RCBlock* c = [[RCBlock alloc] init];
  [c retain_weak_self_in_block_no_retain_cycle];
  return 0;
}

int retain_a_in_block_cycle_bad() {
  RCBlockAA* a = [RCBlockAA new];
  RCBlock* b = [RCBlock new];
  a.b = b;
  b.a_handler = ^(RCBlockAA* b) {
    a.child = nil;
  };
  return 0;
}
