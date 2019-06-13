/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@class NSIndexSet;

@interface NSMutableArray : NSObject

- (void)replaceObjectAtIndex:(NSUInteger)index withObject:(id)anObject;
- (void)removeObjectsAtIndexes:(NSIndexSet*)indexes;
- (void)replaceObjectsAtIndexes:(NSIndexSet*)indexes
                    withObjects:(NSArray*)objects;

@end
