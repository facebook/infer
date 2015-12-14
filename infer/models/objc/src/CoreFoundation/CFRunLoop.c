/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFRunLoopSourceRef __cf_alloc(CFRunLoopSourceRef);
CFRunLoopSourceRef __cf_non_null_alloc(CFRunLoopSourceRef);

CFRunLoopSourceRef CFRunLoopSourceCreate ( CFAllocatorRef allocator,
                                           CFIndex order,
                                           CFRunLoopSourceContext *context ) {
   CFRunLoopSourceRef c;
   return __cf_non_null_alloc(c);
}

CFRunLoopSourceRef CFSocketCreateRunLoopSource ( CFAllocatorRef allocator,
                                                 CFSocketRef s,
                                                 CFIndex order ) {
   CFRunLoopSourceRef c;
   return __cf_non_null_alloc(c);
}
