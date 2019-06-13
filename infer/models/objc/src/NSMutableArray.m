/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#pragma clang diagnostic ignored "-Wdeprecated-objc-isa-usage"

#import "NSMutableArray.h"

@implementation NSMutableArray

- (void)replaceObjectAtIndex:(NSUInteger)index withObject:(id)anObject {
  id a = ((NSObject*)anObject)->isa;
}

- (void)removeObjectsAtIndexes:(NSIndexSet*)indexes {
  id a = ((NSObject*)indexes)->isa;
}

- (void)replaceObjectsAtIndexes:(NSIndexSet*)indexes
                    withObjects:(NSArray*)objects {
  id a = ((NSObject*)indexes)->isa;
  id b = ((NSObject*)objects)->isa;
}

- (void)setObject:(id)anObject atIndexedSubscript:(NSUInteger)index {
  id a = ((NSObject*)anObject)->isa;
}

- (void)addObject:(id)anObject {
  id a = ((NSObject*)anObject)->isa;
}

- (void)insertObject:(id)anObject atIndex:(NSUInteger)index {
  id a = ((NSObject*)anObject)->isa;
}

+ (instancetype)array {
  return [NSMutableArray alloc];
}

@end
