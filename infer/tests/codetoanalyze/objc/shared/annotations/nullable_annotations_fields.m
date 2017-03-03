/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/NSObject.h>

@interface A : NSObject

@end

A* getA();

@implementation A {
  int x;
  A* _Nullable _child;
}
- (int)nullable_field {
  A* a = getA();
  A* child = a->_child;
  return child->x;
}

@end
