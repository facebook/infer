/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject
@property int x;
@end

@implementation A

- (int)addTarget:(A*)target {
  NSAssert(target != nil, @"target must not be nil");
  return target.x;
}

- (int)initWithRequest:(A*)a {
  NSAssert1(a != nil, @"target must not be nil %s", "a");
  return a.x;
}

@end

int test1(A* target) {
  NSCAssert(target != nil, @"target must not be nil");
  return target.x;
}

int test2(A* target) {
  NSCParameterAssert(target);
  return target.x;
}
