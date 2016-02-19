/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

CFRunLoopObserverRef __cf_alloc(CFRunLoopObserverRef);
CFRunLoopSourceRef __cf_non_null_alloc(CFRunLoopSourceRef);

CFRunLoopObserverRef CFRunLoopObserverCreate(
    CFAllocatorRef allocator,
    CFOptionFlags activities,
    Boolean repeats,
    CFIndex order,
    CFRunLoopObserverCallBack callout,
    CFRunLoopObserverContext* context) {
  CFRunLoopObserverRef c;
  return __cf_non_null_alloc(c);
}

CFRunLoopObserverRef CFRunLoopObserverCreateWithHandler(
    CFAllocatorRef allocator,
    CFOptionFlags activities,
    Boolean repeats,
    CFIndex order,
    void (^block)(CFRunLoopObserverRef observer, CFRunLoopActivity activity)) {
  CFRunLoopObserverRef c;
  return __cf_non_null_alloc(c);
}
