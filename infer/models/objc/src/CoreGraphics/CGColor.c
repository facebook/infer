/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreGraphics/CoreGraphics.h>

CGColorRef __cf_non_null_alloc(CGColorRef);

void __objc_release_cf(CGColorRef);

void CGColorRelease(CGColorRef color) {
  if (color)
    __objc_release_cf(color);
}

// FB own code

CGColorRef FBColorCreateWithGray(CGFloat gray, CGFloat a) {
  CGColorRef c;
  return __cf_non_null_alloc(c);
}

CGColorRef FBColorCreateWithRGBA(uint8_t r, uint8_t g, uint8_t b, CGFloat a) {
  CGColorRef c;
  return __cf_non_null_alloc(c);
}

CGColorRef FBColorCreateWithRGB(uint8_t r, uint8_t g, uint8_t b) {
  CGColorRef c;
  return __cf_non_null_alloc(c);
}
