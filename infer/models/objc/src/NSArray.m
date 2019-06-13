/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

+ (instancetype)arrayWithObject:(id)anObject {
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
