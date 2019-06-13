/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

@interface NSAssert : NSObject
@property int x;
@end

@implementation NSAssert

- (int)addTarget:(NSAssert*)target {
  NSAssert(target != nil, @"target must not be nil");
  return target.x;
}

- (int)initWithRequest:(NSAssert*)a {
  NSAssert1(a != nil, @"target must not be nil %s", "a");
  return a.x;
}

@end

int test1(NSAssert* target) {
  NSCAssert(target != nil, @"target must not be nil");
  return target.x;
}

int test2(NSAssert* target) {
  NSCParameterAssert(target);
  return target.x;
}
