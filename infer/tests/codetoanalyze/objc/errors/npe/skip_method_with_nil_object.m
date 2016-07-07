/*
 * Copyright (c) 2016 - present Facebook, Inc.
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

- (A*)get_a;

- (NSString*)skip_method;

@end

@implementation A

- (A*)get_a {
  return [A new];
}

- (int)testOk:(A*)person {
  A* personID = [person get_a];
  NSString* lastRecord = [personID skip_method];
  if (lastRecord) {
    personID->x = 6;
    return personID->x;
  } else {
    return 0;
  }
}

- (int)testBug:(A*)person {
  A* personID = [person get_a];
  NSString* lastRecord = [personID skip_method];
  if (lastRecord) {
    return 0;
  } else {
    return personID->x;
  }
}

@end
