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

#import "NSMutableDictionary.h"

@implementation NSMutableDictionary

- (void)removeObjectForKey:(id)aKey {
  id a = ((NSObject*)aKey)->isa;
}

- (void)setObject:(id)object forKeyedSubscript:(id)aKey {
  id a = ((NSObject*)aKey)->isa;
}

- (void)setObject:(id)anObject forKey:(id)aKey {
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
