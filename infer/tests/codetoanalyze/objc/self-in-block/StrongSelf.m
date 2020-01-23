/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/NSObject.h>

@class SelfInBlockTest;

@interface SelfInBlockTestUser

- (void)use_self_in_block_test:(SelfInBlockTest*)test;

- (void)use_self_in_block_test_nullable:(int)x
                                    and:(_Nullable SelfInBlockTest*)test;
@end

@interface A

@property(nonatomic, strong) id image;

@end

@interface SelfInBlockTest : NSObject

@property(nonatomic, weak) SelfInBlockTestUser* user;

- (void)foo;

- (void)bar;

@end

void m(SelfInBlockTest* obj) {}

void m2(_Nullable SelfInBlockTest* obj) {}

@implementation SelfInBlockTest {
  int x;
}

- (void)foo {
}

- (void)bar {
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

- (void)strongSelfNoCheckNotWeakSelf_good {
  __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    return strongSelf->x;
  };
}

- (void)strongSelfNoCheck_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    [strongSelf foo];
    return 0;
  };
}

// very unlikely pattern, but still complies with invariant:
// any use of strongSelf is bad unless checked for null beforehand
- (void)strongSelfNoCheck2_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    __strong __typeof(weakSelf) strongSelf2 = strongSelf;
    return 0;
  };
}

- (void)strongSelfCheckOnce_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf) {
      [strongSelf foo];
      int x = strongSelf->x;
    } else {
      strongSelf->x; // bug here
      [strongSelf foo]; // no bug here
      m(strongSelf); // bug here
      m2(strongSelf); // no bug here because of _Nullable annotation
      [strongSelf.user use_self_in_block_test:strongSelf]; // bug here
      [strongSelf.user
          use_self_in_block_test_nullable:1
                                      and:strongSelf]; // no bug here because of
                                                       // _Nullable annotation
    }
    [strongSelf foo];
    if (strongSelf != nil) {
      [strongSelf foo];
      int x = strongSelf->x;
    }
    return 0;
  };
}

- (void)strongSelfCheck2_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (!strongSelf) {
      return 0;
    } else {
      [strongSelf foo];
    }
    return 0;
  };
}

- (void)strongSelfCheck3_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    int y = strongSelf->x;
    if (strongSelf == nil) {
      return 0;
    }
    return strongSelf->x;
  };
}

- (void)strongSelfCheck4_good {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (!strongSelf) {
    } else {
    }
    return 0;
  };
}

- (void)strongSelfNotCheck5_good:(A*)a {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)() = ^() {
    __strong __typeof(weakSelf) strongSelf = weakSelf;
    if (strongSelf && a.image) {
      int x = strongSelf->x; // no bug here
    }
    return 0;
  };
}

- (void)wekSelfMultiple_bad {
  __weak __typeof(self) weakSelf = self;
  int (^my_block)(BOOL) = ^(BOOL isTapped) {
    [weakSelf foo];
    [weakSelf bar];
    return 0;
  };
}
@end
