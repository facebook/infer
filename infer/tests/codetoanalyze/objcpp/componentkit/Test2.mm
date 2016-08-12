/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

// This import alone shouldn't trigger lint rules, since its impl is not in
// this translation unit.
// Mimic importing CKComponnet
@interface CKComponent : NSObject
@end
@implementation CKComponent
@end

// Mimic importing CKCompositeComponnet
@interface CKCompositeComponent : NSObject
+ (instancetype)newWithComponent:(CKComponent*)component;
@end
@implementation CKCompositeComponent
+ (instancetype)newWithComponent:(CKComponent*)component {
  return nil;
}
@end

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
