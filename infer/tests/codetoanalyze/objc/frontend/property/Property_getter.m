/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

struct my_struct {
  int x;
  int y;
};

@interface A : NSObject
@property int x;
@property(nonatomic, readwrite) struct my_struct s;
@end

@implementation A

- (int)addTarget:(A*)target {
  return target.x;
}

+ (int)getMyStructField:(A*)target my_struct:(struct my_struct)my_struct {
  target.s = my_struct;
  return target.s.x;
}

@end
