/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>
#import <CoreGraphics/CGGeometry.h>
#import <CoreGraphics/CGColor.h>

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
