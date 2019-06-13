/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#import <CoreGraphics/CoreGraphics.h>

@interface A : NSObject
+ (int)bar;
+ (int)scale;
@end
@implementation A

+ (int)bar {
  return 17;
}

+ (int)scale {
  return 19;
}
@end

int foo() { return 23; }

static const int kInsets = foo(); // Error

static float kPadding = [A bar] ? 10.0 : 11.0; // Error

static const float kLineSize = 1 / [A scale]; // Error

static const float ok = 37;

const NSRange NSNotFoundRange =
    (NSRange){.location = NSNotFound, .length = 0}; // OK

const static CFRange FBCFNotFoundRange =
    (CFRange){.location = kCFNotFound, .length = 7}; // OK

const CGPoint offset = CGPointMake(0, 0); // OK

constexpr double square(double x) { return x * x; }

const int dmv = 17;
constexpr double max1 = 1.4 * square(dmv); // OK

void bla() {
  static const int kInsets = foo(); // OK

  static float kPadding = [A bar] ? 10.0 : 11.0; // OK

  static const float kLineSize = 1 / [A scale]; // OK

  static const float ok = 37;
}
