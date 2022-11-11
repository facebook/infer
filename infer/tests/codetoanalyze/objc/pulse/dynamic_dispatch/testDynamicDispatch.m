/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import "MyViewController.h"

#import <Foundation/NSObject.h>

void testDynamicDispatch_NPEBad() {
  MyViewController* num = createMyViewController(3);
  int* number = [num getViewControllerNumber];
  *number = 0;
}
