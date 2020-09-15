/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

@interface SkipMethodNilA : NSObject {
  int x;
}

- (SkipMethodNilA*)get_a;

- (NSString*)skip_method;

@end

@implementation SkipMethodNilA

- (SkipMethodNilA*)get_a {
  return [SkipMethodNilA new];
}

- (int)testOk:(SkipMethodNilA*)person {
  SkipMethodNilA* personID = [person get_a];
  NSString* lastRecord = [personID skip_method];
  if (lastRecord) {
    personID->x = 6;
    return personID->x;
  } else {
    return 0;
  }
}

- (int)testBug:(SkipMethodNilA*)person {
  SkipMethodNilA* personID = [person get_a];
  NSString* lastRecord = [personID skip_method];
  if (lastRecord) {
    return 0;
  } else {
    return personID->x;
  }
}

@end
