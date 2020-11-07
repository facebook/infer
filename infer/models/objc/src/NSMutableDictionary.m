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
  id a = ((NSObject*)aKey)->isa;
}

- (void)setObject:(id)object forKeyedSubscript:(id<NSCopying>)aKey {
  id a = ((NSObject*)aKey)->isa;
}

- (void)setObject:(id)anObject forKey:(id<NSCopying>)aKey {
  id a = ((NSObject*)anObject)->isa;
  id b = ((NSObject*)aKey)->isa;
}

+ (instancetype)dictionary {
  return [NSMutableDictionary alloc];
}

+ (NSMutableDictionary*)dictionaryWithSharedKeySet:(id)keyset {
  id a = ((NSObject*)keyset)->isa;
}

@end
