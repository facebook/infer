/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <CoreMedia/CoreMedia.h>
#import <Foundation/Foundation.h>
#import <ImageIO/ImageIO.h>

CFDictionaryRef __cf_non_null_alloc(CFDictionaryRef);

CFDictionaryRef __cf_alloc(CFDictionaryRef);

CFDictionaryRef CFDictionaryCreate(
    CFAllocatorRef allocator,
    const void** keys,
    const void** values,
    CFIndex numValues,
    const CFDictionaryKeyCallBacks* keyCallBacks,
    const CFDictionaryValueCallBacks* valueCallBacks) {
  CFDictionaryRef c;
  return __cf_alloc(c);
}

CFDictionaryRef CFDictionaryCreateCopy(CFAllocatorRef allocator,
                                       CFDictionaryRef theDict) {
  CFDictionaryRef c;
  return __cf_alloc(c);
}

CFDictionaryRef CMCopyDictionaryOfAttachments(CFAllocatorRef allocator,
                                              CMAttachmentBearerRef target,
                                              CMAttachmentMode attachmentMode) {
  CFDictionaryRef c;
  return __cf_alloc(c);
}

CFDictionaryRef CFHTTPMessageCopyAllHeaderFields(CFHTTPMessageRef message) {
  CFDictionaryRef c;
  return __cf_alloc(c);
}

CFDictionaryRef CMTimeCopyAsDictionary(CMTime time, CFAllocatorRef allocator) {
  CFDictionaryRef c;
  return __cf_alloc(c);
}
