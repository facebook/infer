/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
