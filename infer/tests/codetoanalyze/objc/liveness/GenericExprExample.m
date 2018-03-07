/*
 * Copyright (c) 2018 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#import <Foundation/Foundation.h>

static float generic_expr_ok() {
  NSString* currentValue = [NSString new];
  return _Generic(0.2f, float : [currentValue floatValue]);
};
