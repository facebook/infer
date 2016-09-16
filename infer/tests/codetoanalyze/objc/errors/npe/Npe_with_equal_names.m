/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface EqualNamesA : NSObject {
 @public
  int x;
}

+ (EqualNamesA*)meth;

@property(nonatomic, readonly) EqualNamesA* meth;

@end

@implementation EqualNamesA

+ (EqualNamesA*)meth {
  return [EqualNamesA new];
}

- (EqualNamesA*)meth {
  return nil;
}

@end

int EqualNamesTest() {
  EqualNamesA* para = [EqualNamesA new];
  EqualNamesA* a = [para meth];
  return a->x;
}

int EqualNamesTest2(EqualNamesA* para) {
  EqualNamesA* a = [EqualNamesA meth];
  return a->x;
}
