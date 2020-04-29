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
  // null dereference, segfault
  my_block();
}

- (void)foo {

  void (^my_block)(void) = ^() {
  };
  my_block = NULL;
  my_block(); // Null deref
}

- foo2:(void (^)(void))my_block {
  // ok to call this block!
  if (my_block != nil) {
    my_block();
  }
}

- (void)foo3:(void (^)(void))my_block {

  my_block = NULL;
  my_block(); // Null deref
}

- (void)foo4:(void (^)(void))my_block_param {

  void (^my_block)(void) = ^() {
  };
  my_block = NULL;
  my_block_param = my_block;
  my_block_param(); // Null deref
}

- (void)foo5:(void (^)(void))my_block_param {

  void (^my_block)(void) = ^() {
  };
  my_block_param = my_block;
  my_block_param(); // No error here
}

- (void)foo6:(BOOL)a block_param:(void (^)(void))block_param {

  void (^my_block)(void) = ^() {
    if (block_param)
      block_param(); // No error here
  };

  if (a) {
    [self foo2:^() {
      my_block(); // No error here
    }];
  }
}

- (void)foo7 {

  _block_field(); // Ivar not nullable
}

- (int)check_nullify {
  int i = 7;
  int* x = &i;
  int (^my_block)(void) = ^() {
    return *x; // we'll get a false NPE here if we nullify x too early.
  };
  return my_block();
}

@end
