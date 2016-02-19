/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface A : NSObject

@property(nonatomic, copy) A* child;

@property(nonatomic, retain) A* name;

@property(nonatomic, unsafe_unretained) A* last_name;

- (A*)copy;

@end

@implementation A

- (A*)copy {
  A* other = [[A alloc] init];
  if (other) {
    other->_name = self->_name;
    other->_last_name = self->_last_name;
    other->_child = self->_child;
  }
  return other;
}

@end

int test(A* a2) {
  A* a = [[A alloc] init];
  a.last_name = a2;
  [a release];
  return 0;
}
