/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreGraphics/CoreGraphics.h>

void __objc_release_cf(CGDataProviderRef);

void CGDataProviderRelease(CGDataProviderRef provider) {
  if (provider)
    __objc_release_cf(provider);
}

CGDataProviderRef __cf_alloc(CGDataProviderRef);

CGDataProviderRef __cf_non_null_alloc(CGDataProviderRef);

CGDataProviderRef CGDataProviderCreateWithURL(CFURLRef url) {
  CGDataProviderRef c;
  return __cf_alloc(c);
}

