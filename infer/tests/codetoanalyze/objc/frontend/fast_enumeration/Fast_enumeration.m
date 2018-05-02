/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSArray.h>
#import <Foundation/NSObject.h>
#import <Foundation/NSEnumerator.h>

@interface A : NSObject
@end

@implementation A {
  NSEnumerator* reverseObjectEnumerator;
}

- (int)fast_loop:(NSArray*)items {
  int size = 0;
  for (NSArray* item in items) {
    size += [item count];
  }
  return size;
}

- (int)while_loop:(NSArray*)items {
  int size = 0;
  NSArray* item = nil;
  while ((item = [items objectAtIndex:3])) {
    size += [item count];
  }
  return size;
}

- (void)fast_loop_no_crash {
  id obj = nil;
  for (obj in reverseObjectEnumerator) {
    [obj copy];
  }
}

@end
