/*
 * Copyright (c) 2016 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>
#import "GetterExample.h"

int should_have_div0() {
  GetterExample* a = [[GetterExample alloc] init];
  a.name = 5;
  return 1 / (a.name - 5);
}
