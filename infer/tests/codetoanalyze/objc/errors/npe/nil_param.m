/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
