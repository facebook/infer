/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFMutableSetRef __cf_alloc(CFMutableSetRef);
CFMutableSetRef __cf_non_null_alloc(CFMutableSetRef);

CFMutableSetRef CFSetCreateMutable(CFAllocatorRef allocator,
                                   CFIndex capacity,
                                   const CFSetCallBacks* callBacks) {
  CFMutableSetRef c;
  return __cf_alloc(c);
}
