/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
