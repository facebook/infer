/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreText/CTFont.h>

CTFontRef __cf_alloc(CTFontRef);

CTFontRef CTFontCreateWithName(CFStringRef name,
                               CGFloat size,
                               const CGAffineTransform* matrix) {
  CTFontRef c;
  return __cf_alloc(c);
}
