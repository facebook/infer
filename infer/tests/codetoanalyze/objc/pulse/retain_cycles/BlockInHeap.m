/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface BlockInHeap : NSObject

typedef void (^BlockInHeapHandler)(BlockInHeap* name);

@property(nonatomic, weak) BlockInHeapHandler weakHandler;

@property(nonatomic, strong) BlockInHeapHandler strongHandler;

@property(nonatomic, strong) BlockInHeap* child;

@end

@implementation BlockInHeap

- (void)assign_weak_handler_to_ivar {
  self.weakHandler = ^(BlockInHeap* b) {
    self->_child = b;
  };
}

- (void)assign_strong_handler_to_ivar_bad {
  self.strongHandler = ^(BlockInHeap* b) {
    self->_child = b;
  };
}
@end

int weak_handler_retain_cycle_ok() {
  BlockInHeap* c = [[BlockInHeap alloc] init];
  [c assign_weak_handler_to_ivar];
  BlockInHeap* b = [[BlockInHeap alloc] init];
  c.weakHandler(b);
  return 5;
}

int strong_handler_retain_cycle_bad() {
  BlockInHeap* c = [[BlockInHeap alloc] init];
  [c assign_strong_handler_to_ivar_bad];
  BlockInHeap* b = [[BlockInHeap alloc] init];
  c.strongHandler(b);
  return 5;
}
