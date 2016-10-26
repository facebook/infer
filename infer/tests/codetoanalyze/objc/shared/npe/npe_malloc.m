/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>
#import <stdlib.h>

struct Person {
  int x;
  int y;
};

@interface NpeMallocC : NSObject

@end

@implementation NpeMallocC

- (struct Person*)test {
  struct Person* person = (struct Person*)malloc(sizeof(struct Person));
  person->x = 10;
  return person;
}

@end
