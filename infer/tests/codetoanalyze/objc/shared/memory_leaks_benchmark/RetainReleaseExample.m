/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface RRA : NSObject

@end

@implementation RRA

- init {
  return self;
}

@end

void retain_release_test() {
  RRA* a = [[RRA alloc] init];
  [a retain];
  [a release];
}
