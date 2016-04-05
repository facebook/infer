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

- (void)setObject:(id)object forKeyedSubscript:(id)aKey {
  NSObject* key = (NSObject*)aKey;
  id isa2 = key->isa;
}

- (void)setObject:(id)anObject forKey:(id)aKey {
  NSObject* obj = (NSObject*)anObject;
  id isa = obj->isa;
  NSObject* key = (NSObject*)aKey;
  id isa2 = key->isa;
}

+ (instancetype)dictionary {
  return [NSMutableDictionary alloc];
}

@end
