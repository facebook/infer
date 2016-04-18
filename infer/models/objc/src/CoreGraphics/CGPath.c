/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreGraphics/CoreGraphics.h>
#import <Foundation/Foundation.h>

CGPathRef __cf_non_null_alloc(CGPathRef);
void __objc_release_cf(CGPathRef);

void CGPathRelease(CGPathRef path) {
  if (path)
    __objc_release_cf(path);
}

