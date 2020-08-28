/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#include <Foundation/Foundation.h>

@interface BlockAsReceiver : NSObject

@end

@implementation BlockAsReceiver

static bool _isAppStartingUp() { return true; }

/* This program would crash if we would specialize NSObject.copy
here because it would become a virtual call without receiver,
as the receiver is the block parameter. Blocks as also objects! */
static void setupTimerOk() {
  static dispatch_once_t onceToken;
  dispatch_once(&onceToken, ^{
    dispatch_block_t _timerBlock = [^{
      if (_isAppStartingUp()) {
        return;
      }
    } copy];
  });
}

@end
