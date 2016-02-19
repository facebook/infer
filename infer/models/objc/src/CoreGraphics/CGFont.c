/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGFontRef);

CGFontRef __cf_alloc(CGFontRef);

void CGFontRelease(CGFontRef font) {
  if (font)
    __objc_release_cf(font);
}

CGFontRef CGFontCreateWithDataProvider(CGDataProviderRef provider) {
  CGFontRef c;
  return __cf_alloc(c);
}
