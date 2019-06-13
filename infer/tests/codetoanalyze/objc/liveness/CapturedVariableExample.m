/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface A : NSObject
@end

typedef void (^MyBlock)();

@interface C : NSObject

+ (void)use_block:(MyBlock)block;

@end

@implementation A

+ (A*)g {
  __block A* a;
  [C use_block:^() {
    a = [A new];
  }];
  return a;
};

@end
