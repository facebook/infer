/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


#import <ImageIO/ImageIO.h>
#import <CoreMedia/CoreMedia.h>
#import <Foundation/Foundation.h>

CFDictionaryRef __cf_non_null_alloc(CFDictionaryRef);

CFDictionaryRef __cf_alloc(CFDictionaryRef);

CFDictionaryRef CGImageSourceCopyPropertiesAtIndex ( CGImageSourceRef isrc,
                                                     size_t index,
                                                     CFDictionaryRef options )
{
    CFDictionaryRef c;
    return __cf_non_null_alloc(c);
}


CFDictionaryRef CFDictionaryCreate ( CFAllocatorRef allocator,
                                        const void **keys,
                                        const void **values,
                                        CFIndex numValues,
                                        const CFDictionaryKeyCallBacks *keyCallBacks,
                                        const CFDictionaryValueCallBacks *valueCallBacks ) {
    CFDictionaryRef c;
    return __cf_alloc(c);
}

CFDictionaryRef CFDictionaryCreateCopy ( CFAllocatorRef allocator,
                                         CFDictionaryRef theDict ) {
    CFDictionaryRef c;
    return __cf_alloc(c);
}

CFDictionaryRef CFNetworkCopySystemProxySettings ( void ) {
    CFDictionaryRef c;
    return __cf_non_null_alloc(c);
}

CFDictionaryRef CGImageSourceCopyProperties ( CGImageSourceRef isrc, CFDictionaryRef options ) {
    CFDictionaryRef c;
    return __cf_non_null_alloc(c);
}

CFDictionaryRef CMCopyDictionaryOfAttachments ( CFAllocatorRef allocator,
                                                CMAttachmentBearerRef target,
                                                CMAttachmentMode attachmentMode ) {
    CFDictionaryRef c;
    return __cf_alloc(c);
}

CFDictionaryRef CFHTTPMessageCopyAllHeaderFields ( CFHTTPMessageRef message ) {
    CFDictionaryRef c;
    return __cf_alloc(c);
}

CFDictionaryRef CNCopyCurrentNetworkInfo ( CFStringRef interfaceName ) {
    CFDictionaryRef c;
    return __cf_non_null_alloc(c);
}

CFDictionaryRef CMTimeCopyAsDictionary (CMTime time,
                                        CFAllocatorRef allocator ) {
  CFDictionaryRef c;
  return __cf_alloc(c);
}
