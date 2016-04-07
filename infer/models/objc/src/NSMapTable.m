/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import "NSMapTable.h"

@implementation NSMapTable

- (id)objectForKey:(id)aKey {
  if (aKey == NULL)
    return NULL;
  return [NSObject alloc];
}

@end
