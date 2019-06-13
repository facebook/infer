/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void __free_cf(CFTypeRef item);

void CFRelease(CFTypeRef item) { __free_cf(item); }

CFRunLoopObserverRef getAngelicObject();

CFRunLoopObserverRef CFRunLoopObserverCreateWithHandler(
    CFAllocatorRef allocator,
    CFOptionFlags activities,
    Boolean repeats,
    CFIndex order,
    void (^block)(CFRunLoopObserverRef observer, CFRunLoopActivity activity)) {
  // We need to skip allocation for this function since we currently don't
  // handle object finalizers in objc. This is to avoid false positive memory
  // leaks when the 'observer' is correctly freed in the block argument.

  // undefined function will get skipped and thus make
  // CFRunLoopObserverCreateWithHandler return an angelic object
  return getAngelicObject();
}
