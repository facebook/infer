/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "NSArray.h"

@implementation NSArray

+ (instancetype)array {
  return [NSArray alloc];
}

+ (instancetype)arrayWithObject:(char*)anObject {
  char _ = *anObject;
  return [NSArray alloc];
}

@end
