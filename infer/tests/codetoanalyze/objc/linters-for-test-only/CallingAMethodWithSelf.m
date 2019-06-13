/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
