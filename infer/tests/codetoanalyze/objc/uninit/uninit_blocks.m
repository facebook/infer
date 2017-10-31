/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@interface A : NSObject
@end

typedef void (^MyBlock)();

@interface C : NSObject

+ (void)use_block:(MyBlock)block;

@end

@implementation A

+ (int)ok1 {
  __block int a;
  [C use_block:^() {
    a = 10;
  }];
  return a;
};

+ (int)bad1 {
  int a;
  [C use_block:^() {
    int x = 0;
  }];
  return a;
};

+ (instancetype)ok2 {
  static id sharedInstance;
  ^{
    sharedInstance = [[self alloc] init];
  }();

  return sharedInstance;
}

@end
