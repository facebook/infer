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

CGImageSourceRef __cf_non_null_alloc(CGImageSourceRef);
CGImageSourceRef __cf_alloc(CGImageSourceRef);
void __objc_release_cf(CGImageSourceRef);

CGImageSourceRef CGImageSourceCreateWithURL(CFURLRef url,
                                            CFDictionaryRef options) {
  CGImageSourceRef c;
  return __cf_alloc(c);
}


void CGImageSourceRelease(CGImageSourceRef image) {
  if (image)
    __objc_release_cf(image);
}
