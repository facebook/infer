/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import "ViewController.h"
#import <Foundation/NSObject.h>

int retain_cycle_weak_good() {
  ViewController* controller = [ViewController new];
  return 0;
}
