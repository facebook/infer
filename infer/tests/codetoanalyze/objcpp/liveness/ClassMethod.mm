/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

struct A {
  Class ac;
  NSObject* a;
};

@interface I : NSObject
+ (instancetype)newWithA:(const A&)a;
@end

@implementation I
+ (instancetype)newWithA:(const A&)a {
  return [self alloc];
}
@end

id cMehtod() {
  NSObject* o = [NSObject new];
  return [I newWithA:{[I class], o}];
}
