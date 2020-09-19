/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import "no_arc_callee.h"

@implementation NoArcCallee

+ (NoArcCallee*)allocObject {
  return [NoArcCallee alloc];
}

/* Since the checker cares about calling `autorelease` only, we define the
   following functions as returning a new object simply. */

+ (NoArcCallee*)newObject {
  return [[NoArcCallee alloc] init];
}

+ (NoArcCallee*)copyObject:(NoArcCallee*)obj {
  return [[NoArcCallee alloc] init];
}

+ (NoArcCallee*)mutableCopyObject:(NoArcCallee*)obj {
  return [[NoArcCallee alloc] init];
}

+ (NoArcCallee*)giveMeObject {
  return [[[NoArcCallee alloc] init] autorelease];
}

@end
