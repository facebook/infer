/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#pragma clang diagnostic ignored "-Wdeprecated-objc-isa-usage"

#import "NSMutableArray.h"

@implementation NSMutableArray

- (void)setObject:(id)anObject atIndexedSubscript:(NSUInteger)index {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
}

- (void)addObject:(id)anObject {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
}

- (void)insertObject:(id)anObject atIndex:(NSUInteger)index {
    NSObject *obj = (NSObject*)anObject;
    id isa = obj->isa;
}

+ (instancetype)array {
    return [NSMutableArray alloc];
}


@end
