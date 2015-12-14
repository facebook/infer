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

CGImageDestinationRef __cf_non_null_alloc(CGImageDestinationRef);
CGImageDestinationRef __cf_alloc(CGImageDestinationRef);

CGImageDestinationRef CGImageDestinationCreateWithURL (CFURLRef url,
                                                       CFStringRef type,
                                                       size_t count,
                                                       CFDictionaryRef options) {
    CGImageDestinationRef c;
    return __cf_non_null_alloc(c);
}

CGImageDestinationRef CGImageDestinationCreateWithData ( CFMutableDataRef data,
                                                         CFStringRef type,
                                                         size_t count,
                                                         CFDictionaryRef options) {
    CGImageDestinationRef c;
    return __cf_non_null_alloc(c);
}
