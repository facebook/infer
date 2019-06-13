/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void runBlock(__attribute__((noescape)) dispatch_block_t block) { block(); }

void safe() {

  int x = 5;
  int& xref = x;
  runBlock(^{
    xref = 6;
  }); // safe
}

void unsafe() {
  int x = 5;
  int& xref = x;
  dispatch_queue_t queue = dispatch_queue_create("queue", NULL);
  dispatch_async(queue, ^{
    // Infer flags the following:
    xref = 6; // CRASH! local has gone away
  });
}

void both_safe_and_unsafe() {

  int x = 5;
  int& xref = x;
  dispatch_block_t my_block = ^{
    xref = 6;
  };
  dispatch_queue_t queue = dispatch_queue_create("queue", NULL);

  runBlock(my_block); // safe
  dispatch_async(queue, my_block); // unsafe
}
