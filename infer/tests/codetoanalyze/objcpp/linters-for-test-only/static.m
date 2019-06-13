/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

@end

@implementation A

+ (instancetype)test {
  static id sharedInstance;
  ^{
    sharedInstance = [[self alloc] init];
  }();

  return sharedInstance;
}

+ (int)testOK {
  static int i;

  ^{
    int j = 0;
  }();

  return i;
}

@end
