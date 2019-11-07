/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/NSObject.h>

@interface SelfInBlockTest : NSObject

- (void)foo;

@end

@implementation SelfInBlockTest {
  int x;
}

- (void)foo {
}

- (void)mixSelfWeakSelf_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = self->x; // bug here
      [self foo];
    }
    return 0;
  };
}

@end
