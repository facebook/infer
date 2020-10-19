/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

NSInteger block_multiply_array_linear(NSArray* array) {
  NSInteger (^sum_array)(NSArray*) = ^(NSArray* array) {
    NSInteger n = 0;
    for (id value in array) {
      n += [value integerValue];
    }
    return n;
  };

  return sum_array(array);
}

typedef void (^BlockA)(void);
void loop_linear(int x) {
  for (int i = 0; i < x; i++) {
  }
}

void runBlockA(BlockA block) { block(); }

void doBlockA_linear(int a) {
  BlockA block = ^{
    loop_linear(a);
  };
  runBlockA(block);
}

void doBlockA_direct_block_linear(int a) {
  runBlockA(^{
    loop_linear(a);
  });
}

void wrapper_runBlockA(BlockA block) { runBlockA(block); }

void call_wrapper_runBlockA_linear(int a) {
  wrapper_runBlockA(^{
    loop_linear(a);
  });
}
