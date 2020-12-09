/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/NSObject.h>

@interface Uninit : NSObject
@end

@implementation Uninit

- (void)capture_in_closure_ok {
  __block BOOL x;

  void (^block)() = ^() {
    x = true;
  };
}

- (BOOL)capture_in_closure_bad {
  __block BOOL x;

  void (^block)() = ^() {
    x = true;
  };

  return x;
}

- (BOOL)set_in_closure_ok {
  __block BOOL x;

  void (^block)() = ^() {
    x = true;
  };

  block();
  return x;
}

- (BOOL)not_set_in_closure_bad {
  __block BOOL x;

  void (^block)() = ^() {
  };

  block();
  return x;
}

- (BOOL)use_in_closure_bad_FN {
  __block BOOL x;

  void (^block)() = ^() {
    BOOL y = x;
  };

  block();
}

@end
