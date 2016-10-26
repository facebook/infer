/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

@interface NilParamA : NSObject {
  int x;
}
@end

@implementation NilParamA

- (void)test2 {
  self->x = 1;
}

- (void)test1:(NilParamA*)other {

  [other test2];
}

@end

int NilParamMain() {

  NilParamA* a = [NilParamA alloc];
  [a test1:nil];
  [a release];
  return 0;
}
