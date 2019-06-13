/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

static float generic_expr_ok() {
  NSString* currentValue = [NSString new];
  return _Generic(0.2f, float : [currentValue floatValue]);
};
