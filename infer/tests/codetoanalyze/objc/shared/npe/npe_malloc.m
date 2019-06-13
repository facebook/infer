/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
