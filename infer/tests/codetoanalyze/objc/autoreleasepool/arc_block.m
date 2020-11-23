/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "no_arc_callee.h"

@interface ArcBlock : NSObject
@end

@implementation ArcBlock

+ (void)callIndexOfObjectPassingTest_linear:(NSArray*)x {
  int i = [x indexOfObjectPassingTest:^BOOL(
                 NSObject* obj, NSUInteger idx, BOOL* stop) {
    NoArcCallee* o = [NoArcCallee giveMeObject];
    return false;
  }];
}

+ (void)callIndexOfObjectPassingTest_param_linear:(NSArray*)x {
  BOOL (^b)(NSObject*, NSUInteger, BOOL*) =
      ^BOOL(NSObject* obj, NSUInteger idx, BOOL* stop) {
        NoArcCallee* o = [NoArcCallee giveMeObject];
        return false;
      };
  int i = [x indexOfObjectPassingTest:b];
}

BOOL x;

- (void)conditionalRunBlock:(dispatch_block_t)block {
  if (x) {
    block();
  }
}

- (void)call_ConditionalRunBlock_linear:(int)k {
  int i = 0;
  while (i < k) {
    [self conditionalRunBlock:^{
      NoArcCallee* o = [NoArcCallee giveMeObject];
    }];
    i++;
  }
}

- (void)callBlock:(dispatch_block_t)block {
  dispatch_block_t local_block = block;
  local_block();
}

- (void)call_CallBlock_linear:(int)k {
  int i = 0;
  while (i < k) {
    [self callBlock:^{
      NoArcCallee* o = [NoArcCallee giveMeObject];
    }];
    i++;
  }
}

@end
