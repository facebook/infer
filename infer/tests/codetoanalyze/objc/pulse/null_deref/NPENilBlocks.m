/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface BlockA : NSObject
@end

@implementation BlockA {

  void (^_block_field)(void);
}

- (void)doSomethingThenCallback:(void (^)(void))my_block {
  // null dereference, segfault if my_block is nil
  my_block();
}

- (void)assignNilBad {

  void (^my_block)(void) = ^() {
  };
  my_block = NULL;
  my_block(); // Null deref
}

- (void)checkNotNilOk:(void (^)(void))my_block {
  // ok to call this block!
  if (my_block != nil) {
    my_block();
  }
}

- (void)paramAssignNilBad:(void (^)(void))my_block {

  my_block = NULL;
  my_block(); // Null deref
}

- (void)paramReassignNilBad:(void (^)(void))my_block_param {

  void (^my_block)(void) = ^() {
  };
  my_block = NULL;
  my_block_param = my_block;
  my_block_param(); // Null deref
}

- (void)assignEmptyBlockOk:(void (^)(void))my_block_param {

  void (^my_block)(void) = ^() {
  };
  my_block_param = my_block;
  my_block_param(); // No error here
}

- (void)checkNotNilBlockAsArgOk:(BOOL)a
                    block_param:(void (^)(void))block_param {

  void (^my_block)(void) = ^() {
    if (block_param)
      block_param(); // No error here
  };

  if (a) {
    [self checkNotNilOk:^() {
      my_block(); // No error here
    }];
  }
}

- (void)FN_ivarNilBlockBad {
  _block_field(); // Ivar not nullable
}

- (int)check_nullifyOk {
  int i = 7;
  int* x = &i;
  int (^my_block)(void) = ^() {
    return *x; // we'll get a false NPE here if we nullify x too early.
  };
  return my_block();
}

@end

void calldoSomethingThenCallbackOk() {
  BlockA* blockA = [BlockA alloc];
  void (^my_block)(void) = ^() {
  };
  [blockA doSomethingThenCallback:my_block];
}

void calldoSomethingThenCallbackWithNilBad() {
  BlockA* blockA = [BlockA alloc];
  [blockA doSomethingThenCallback:nil];
}

void nilBlockCallCFuntionBad() {
  void (^my_block)(void) = ^() {
  };
  my_block = NULL;
  my_block();
}
