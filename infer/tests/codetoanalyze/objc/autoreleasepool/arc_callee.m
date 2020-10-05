/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "arc_callee.h"

@implementation ArcCallee

+ (ArcCallee*)allocObject {
  return [ArcCallee alloc];
}

+ (ArcCallee*)newObject {
  return [ArcCallee new];
}

/* Since the checker cares about calling `autorelease` only, we define the
   following functions as returning a new object simply. */

+ (ArcCallee*)copyObject:(ArcCallee*)obj {
  return [ArcCallee new];
}

+ (ArcCallee*)mutableCopyObject:(ArcCallee*)obj {
  return [ArcCallee new];
}

+ (ArcCallee*)giveMeObject {
  return [ArcCallee new];
}

+ (int)giveMeInt {
  return 100;
}

+ (NSString*)giveTaggedPointerString {
  return [NSString new];
}

+ (NSNumber*)giveTaggedPointerNumber {
  return [NSNumber new];
}

+ (NSIndexPath*)giveTaggedPointerIndexPath {
  return [NSIndexPath new];
}

+ (NSIndexSet*)giveTaggedPointerIndexSet {
  return [NSIndexSet new];
}

+ (NSDate*)giveTaggedPointerDate {
  return [NSDate new];
}

@end
