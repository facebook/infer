/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface B : NSObject

@end

@implementation B

- (instancetype)init {
  return nil;
}

@end

@interface A : B

@end

@implementation A {
  int a;
}

- (instancetype)init {
  self = [super init];
  self->a = 4;
  return self;
}

@end

int main(int argc, char* argv[]) {
  @autoreleasepool {
    __unused id a = [A new];
  }
}
