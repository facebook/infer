/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface BSuper : NSObject

@end

@implementation BSuper

- (instancetype)init {
  return nil;
}

@end

@interface ASuper : BSuper

@end

@implementation ASuper {
  int a;
}

- (instancetype)init {
  self = [super init];
  self->a = 4;
  return self;
}

@end

int super_example_main(int argc, char* argv[]) {
  @autoreleasepool {
    __unused id a = [ASuper new];
  }
}
