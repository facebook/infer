/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
