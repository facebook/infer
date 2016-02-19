/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreGraphics/CoreGraphics.h>
#import <ImageIO/ImageIO.h>
#import <QuartzCore/QuartzCore.h>

void __objc_release_cf(CGContextRef);

void CGContextRelease(CGContextRef c) {
  if (c)
    __objc_release_cf(c);
}

CGContextRef __cf_alloc(CGContextRef);

CGContextRef CGBitmapContextCreate(void* data,
                                   size_t width,
                                   size_t height,
                                   size_t bitsPerComponent,
                                   size_t bytesPerRow,
                                   CGColorSpaceRef space,
                                   CGBitmapInfo bitmapInfo) {
  CGContextRef c;
  return __cf_alloc(c);
}
