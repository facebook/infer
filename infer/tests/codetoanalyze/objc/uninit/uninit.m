/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */
#include <Foundation/Foundation.h>

@interface A : NSObject

- (CGSize)sizeThatFits:(int)x;
- (int)init_var_ok;

@end

@implementation A

- (CGSize)sizeThatFits:(int)x {

  CGSize s = CGSizeMake(x, x);

  return s;
}

- (int)init_var_ok {
  const CGSize labelSize = [self sizeThatFits:10];

  return labelSize.height; // Here we should not report uninit
}

typedef NS_ENUM(NSUInteger, SomeEnum) { ValueA, ValueB };

CGColorRef FP_switch_ok(SomeEnum e, CGColorRef defaultcolor) {
  CGColorRef color;

  switch (e) {
    case ValueA:
      color = defaultcolor;
      break;
    case ValueB:
      color = defaultcolor;
      break;
  }
  return color; // false positive because of exausted switch
}

@end
