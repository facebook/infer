/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface A : NSObject {
  int x;
}
@end

@implementation A

- (instancetype)init {
  if ([super self]) {
    self->x = 10;
  }
  return self;
}

@end
