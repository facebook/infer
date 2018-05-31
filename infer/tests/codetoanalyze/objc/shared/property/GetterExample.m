/*
 * Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import "GetterExample.h"

int should_have_div0() {
  GetterExample* a = [[GetterExample alloc] init];
  a.name = 5;
  return 1 / (a.name - 5);
}
