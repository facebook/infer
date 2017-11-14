/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

@interface CallingAMethodWithSelfBase
- (void)testView;
@end

@implementation CallingAMethodWithSelfBase

- (void)testView {
  int k = 0;
}
@end

@interface CallingAMethodWithSelf : CallingAMethodWithSelfBase

- (void)methodThatShallNotComplain;

- (void)methodThatShallComplain;

- (void)testView;
@end

@implementation CallingAMethodWithSelf

- (void)methodThatShallBeOkayVoid {
  testView();
}

- (void)methodThatShallBeOkaySuper {
  [super testView];
}

- (void)methodThatShallComplain {
  [self testView];
}

- (void)testView {
  int p = 0;
}
@end
