/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGGradientRef);

void CGGradientRelease(CGGradientRef gradient) {
  if (gradient)
    __objc_release_cf(gradient);
}

CGGradientRef __cf_non_null_alloc(CGGradientRef);
