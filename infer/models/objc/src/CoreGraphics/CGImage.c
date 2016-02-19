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

CGImageRef CGImageSourceCreateImageAtIndex(CGImageSourceRef isrc,
                                           size_t index,
                                           CFDictionaryRef options) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}

CGImageRef CGImageSourceCreateThumbnailAtIndex(CGImageSourceRef isrc,
                                               size_t index,
                                               CFDictionaryRef options) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}

void CGImageRelease(CGImageRef image) {
  if (image)
    __objc_release_cf(image);
}

CGImageRef CGBitmapContextCreateImage(CGContextRef context) {
  CGImageRef c;
  return __cf_alloc(c);
}

CGImageRef CGImageCreateCopy(CGImageRef image) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}

CGImageRef CGImageCreateWithImageInRect(CGImageRef image, CGRect rect) {
  CGImageRef c;
  return __cf_alloc(c);
}

CGImageRef CGImageCreateWithJPEGDataProvider(CGDataProviderRef source,
                                             const CGFloat decode[],
                                             bool shouldInterpolate,
                                             CGColorRenderingIntent intent) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}

CGImageRef CGImageCreateWithPNGDataProvider(CGDataProviderRef source,
                                            const CGFloat decode[],
                                            bool shouldInterpolate,
                                            CGColorRenderingIntent intent) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}

CGImageRef CGImageCreate(size_t width,
                         size_t height,
                         size_t bitsPerComponent,
                         size_t bitsPerPixel,
                         size_t bytesPerRow,
                         CGColorSpaceRef space,
                         CGBitmapInfo bitmapInfo,
                         CGDataProviderRef provider,
                         const CGFloat decode[],
                         bool shouldInterpolate,
                         CGColorRenderingIntent intent) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}

CGImageRef CGImageCreateWithMask(CGImageRef image, CGImageRef mask) {
  CGImageRef c;
  return __cf_non_null_alloc(c);
}
