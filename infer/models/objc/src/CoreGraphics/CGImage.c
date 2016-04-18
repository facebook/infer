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

CGImageRef __cf_non_null_alloc(CGImageRef);
CGImageRef __cf_alloc(CGImageRef);
void __objc_release_cf(CGImageRef);

void CGImageRelease(CGImageRef image) {
  if (image)
    __objc_release_cf(image);
}

CGImageRef CGBitmapContextCreateImage(CGContextRef context) {
  CGImageRef c;
  return __cf_alloc(c);
}

CGImageRef CGImageCreateWithImageInRect(CGImageRef image, CGRect rect) {
  CGImageRef c;
  return __cf_alloc(c);
}
