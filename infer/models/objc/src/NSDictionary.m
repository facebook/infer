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

#import "NSDictionary.h"

@implementation NSDictionary

- (id)objectForKey:(id)aKey {
  if (aKey == NULL)
    return NULL;
  return [NSObject alloc];
}

- (id)objectForKeyedSubscript:(id)aKey {
  if (aKey == NULL)
    return NULL;
  return [NSObject alloc];
}

+ (instancetype)dictionary {
  return [NSDictionary alloc];
}

+ (instancetype)dictionaryWithObject:(id)object forKey:(id)key {
  id a = ((NSObject*)object)->isa;
  id b = ((NSObject*)key)->isa;
  return [NSDictionary alloc];
}

@end
