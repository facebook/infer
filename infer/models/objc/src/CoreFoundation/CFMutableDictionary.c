/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreMedia/CoreMedia.h>
#import <ImageIO/ImageIO.h>

CFMutableDictionaryRef __cf_non_null_alloc(CFMutableDictionaryRef);

CFMutableDictionaryRef __cf_alloc(CFMutableDictionaryRef);

CFMutableDictionaryRef CFDictionaryCreateMutable(
    CFAllocatorRef allocator,
    CFIndex capacity,
    const CFDictionaryKeyCallBacks* keyCallBacks,
    const CFDictionaryValueCallBacks* valueCallBacks) {

  CFMutableDictionaryRef c;
  return __cf_alloc(c);
}
