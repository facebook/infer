/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

#import "FakeComponentKitHeader.h"

@interface SomeClass : NSObject
@end
@implementation SomeClass {
  NSString* _foo;
}

- (instancetype)init {
  if (self = [super init]) {
    NSString* foo = @"HI"; // no error
    _foo = foo;
  }
}
@end
