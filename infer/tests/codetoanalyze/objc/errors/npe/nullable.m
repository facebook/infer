/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSString.h>

@interface A : NSObject {
    @public int x;
}

@end

int derefNullableParamDirect(A * __nullable param) {
  return param->x;
}

int derefNullableParamIndirect(A * __nullable param) {
  A* local = param;
  return local->x;
}

A * derefNullableParamOk(A * __nullable param) {
  if (!param) param = [A new];
  param->x = 7;
  return param;
}
