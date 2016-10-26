/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface ExplicitIvarNameA : NSObject

@property(nonatomic) int x;

@end

@interface ExplicitIvarNameB : NSObject

@property(nonatomic) int y;

@end

@implementation ExplicitIvarNameA

- init {
  return self;
}

@synthesize x;

- (int)testExplicit {
  int* p = 0;
  self->x = 5;
  if (self.x == 5) { // If NPE is found, means that getter is using the correct
    // ivar name x
    // rather than the default _x
    return *p;
  };
}

- (int)testDefaultName {
  int* p = 0;
  ExplicitIvarNameB* b = [[ExplicitIvarNameB alloc] init];
  b.y = 5;
  if (b.y == 5) { // If NPE is found, means that getter is using default name _y
    // that is
    // added to the tenv, so there is no Missing_fld beforehand.
    return *p;
  };
}
@end
