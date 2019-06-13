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
    // return sharedInstance;
  }();

  return sharedInstance;
}

+ (void)test_leak {
  static id sharedInstance;
  ^{
    sharedInstance = [[self alloc] init];
    // return sharedInstance;
  }();
}

+ (instancetype)test2 {
  static id sharedInstance;
  sharedInstance = [[self alloc] init];
  ^{
    // NSLog(@"Passing from block...\n");
    id p = sharedInstance;
  }();

  return sharedInstance;
}

+ (int)test3 {
  static int i;

  ^{
    // NSLog(@"Passing from block...\n");
    i++;
  }();

  return i;
}

@end

int main(int argc, const char* argv[]) { return 0; }
