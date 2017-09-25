/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>
#import <stdlib.h>

CFStringRef __cf_alloc(CFStringRef);

void __get_array_length(const UInt8*);

CFStringRef CFStringCreateWithBytesNoCopy(CFAllocatorRef alloc,
                                          const UInt8* bytes,
                                          CFIndex numBytes,
                                          CFStringEncoding encoding,
                                          Boolean isExternalRepresentation,
                                          CFAllocatorRef contentsDeallocator) {
  CFStringRef c;
  CFStringRef s = __cf_alloc(c);
  if (s) {
    if (bytes) {
      __get_array_length(bytes);
      free(bytes);
    }
  }
  return s;
}
