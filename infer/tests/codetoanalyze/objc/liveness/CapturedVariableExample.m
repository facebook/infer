/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
