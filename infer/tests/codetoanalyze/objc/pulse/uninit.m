/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

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

- (BOOL)dispatch_sync_closure_ok:(dispatch_queue_t)queue {
  __block BOOL x;

  dispatch_sync(queue, ^() {
    x = true;
  });
  return x;
}

- (BOOL)dispatch_sync_variable_closure_ok:(dispatch_queue_t)queue {
  __block BOOL x;

  void (^block)() = ^() {
    x = true;
  };
  dispatch_sync(queue, block);
  return x;
}

@end
