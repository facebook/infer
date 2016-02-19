/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

@property int x;

+ (instancetype)sharedInstance;

@end

@implementation A {
}

+ (instancetype)sharedInstance {
  static dispatch_once_t once;
  static id sharedInstance;
  dispatch_once(&once, ^{
    sharedInstance = [[self alloc] init];
  });
  return sharedInstance;
}

+ (instancetype)trans {
  static id sharedInstance;
  void (^dummy_block)() = ^{
    sharedInstance = [[self alloc] init];
  };
  dummy_block();
  return sharedInstance;
}
@end

int main() {
  A* b = [A sharedInstance];
  int* p = 0;
  if (b == 0)
    return *p;
  else
    return 0;
}
