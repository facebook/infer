/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface Functions : NSObject
@property(strong) void (^block)(void);
@end

@implementation Functions

+ (instancetype)autoUpdating {
  static Functions* a;
  dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    a = [self alloc];
    a.block = ^{
    };
  });
  return a;
}

static void dispatch_once2(dispatch_once_t* predicate, dispatch_block_t block);
+ (instancetype)autoUpdating2 {
  static Functions* a;
  dispatch_once_t onceToken;
  dispatch_once2(&onceToken, ^{
    a = [self alloc];
    a.block = ^{
    };
  });
  return a;
}

@end

int main() {
  Functions* a = [Functions autoUpdating];
  Functions* a2 = [Functions autoUpdating2];
  a.block();
  a2.block(); // NPE here since dispatch_once2 is skipped
}
