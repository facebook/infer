/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface MyClass : NSObject
+ (void)aClassMethod;
- (void)anInstanceMethod;
+ (void)aClassMethod2;
- (int)getX;
@end

@implementation MyClass
+ (void)aClassMethod {
  MyClass* myClass = [self alloc];
}

- (void)anInstanceMethod {
  [MyClass aClassMethod];
}

+ (void)aClassMethod2 {
  [self aClassMethod];
}

- (int)getX {
  return 0;
}

- (void)anInstanceMethod2 {
  [self getX];
}
@end
