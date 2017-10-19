/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import "A.h"

@implementation A {

  int _x;
  NSString* _name;
}

- (instancetype)withMetadata:(NSString*)name {
  self->_x = 5;
  self->_name = name;
  return self;
}

- (int)getX {
  return _x;
}

@end
