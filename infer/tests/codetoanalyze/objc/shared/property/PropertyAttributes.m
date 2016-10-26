/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

@interface PropertyA : NSObject

@property(nonatomic, copy) PropertyA* child;

@property(nonatomic, retain) PropertyA* name;

@property(nonatomic, unsafe_unretained) PropertyA* last_name;

- (PropertyA*)copy;

@end

@implementation PropertyA

- init {
  return self;
}

- (PropertyA*)copy {
  PropertyA* other = [[PropertyA alloc] init];
  if (other) {
    other->_name = self->_name;
    other->_last_name = self->_last_name;
    other->_child = self->_child;
  }
  return other;
}

@end

int test(PropertyA* a2) {
  PropertyA* a = [[PropertyA alloc] init];
  a.last_name = a2;
  [a release];
  return 0;
}
