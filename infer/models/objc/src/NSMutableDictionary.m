/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#pragma clang diagnostic ignored "-Wdeprecated-objc-isa-usage"

#import "NSMutableDictionary.h"

@implementation NSMutableDictionary

- (void)removeObjectForKey:(id)aKey {
  id a = object_getClass(aKey);
}

- (void)setObject:(id)object forKeyedSubscript:(id<NSCopying>)aKey {
  id a = object_getClass(aKey);
}

- (void)setObject:(id)anObject forKey:(id<NSCopying>)aKey {
  id a = object_getClass(anObject);
  id b = object_getClass(aKey);
}

+ (instancetype)dictionary {
  return [NSMutableDictionary alloc];
}

+ (NSMutableDictionary*)dictionaryWithSharedKeySet:(id)keyset {
  id a = object_getClass(keyset);
}

@end
