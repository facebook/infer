/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/NSObject.h>

@interface WeakCapturedA : NSObject

@end

@implementation WeakCapturedA {
  int x;
}

- (void)strongSelfNoCheckNotWeakSelf {
  __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    return strongSelf->x;
  };
}

- (void)strongSelfNoCheck {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    return strongSelf->x;
  };
}

- (void)strongSelfCheck {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf)
      return strongSelf->x;
    else
      return 0;
  };
}

@end
