/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface BlockInHeap : NSObject

typedef void (^BlockInHeapHandler)(BlockInHeap* name);

@property(nonatomic, weak) BlockInHeapHandler handler;

@property(nonatomic, strong) BlockInHeap* child;

@end

@implementation BlockInHeap

- (void)assign_block_to_ivar {
  self.handler = ^(BlockInHeap* b) {
    self->_child = b;
  };
}

@end
// no retain cycle because handler is a weak pointer.
int block_in_heap_executed_after_bi_abduction_ok_no_retain_cycle() {
  BlockInHeap* c = [[BlockInHeap alloc] init];
  [c assign_block_to_ivar];
  BlockInHeap* b = [[BlockInHeap alloc] init];
  c.handler(b);
  return 5;
}

int block_in_heap_executed_after_bi_abduction_ok_test() {
  if (block_in_heap_executed_after_bi_abduction_ok_no_retain_cycle() == 5) {
    int* p = 0;
    return *p;
  } else {
    int* p = 0;
    return *p;
  }
}
