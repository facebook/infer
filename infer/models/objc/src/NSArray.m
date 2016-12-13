/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "NSArray.h"

void __infer_assume(bool cond);

@implementation NSArray {

 @private
  id elementData[10];
}

- (NSArray*)arrayByAddingObject:(id)anObject {
  id a = ((NSObject*)anObject)->isa;
  return [NSArray alloc];
}

+ (instancetype)array {
  return [NSArray alloc];
}

+ (instancetype)arrayWithObject:(char*)anObject {
  id a = ((NSObject*)anObject)->isa;
  return [NSArray alloc];
}

- (id)objectAtIndexedSubscript:(NSUInteger)idx {
  id obj = elementData[idx];
  __infer_assume(obj != nil);
  return obj;
}

- (int)count {
  if (self == nil) {
    return 0;
  } else {
    sizeof(elementData);
  }
}

@end
