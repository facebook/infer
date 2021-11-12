/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

// We don't treat properly the fact that the observer is released in a callback,
// however we don't report a leak here anyway because observer is passed to
// CFRunLoopAddObserver.
void allocAndReleaseInBlockNoLeakOk() {
  void (^callback)(CFRunLoopObserverRef obs, CFRunLoopActivity activity) =
      ^(CFRunLoopObserverRef obs, CFRunLoopActivity activity) {
        // We should check if the object is released properly, see
        // infer/models/objc/src/CoreFoundation.c
        CFRelease(obs);
      };

  CFRunLoopObserverRef observer = CFRunLoopObserverCreateWithHandler(
      NULL, kCFRunLoopBeforeWaiting, NO, UINT_MAX, callback);
  CFRunLoopAddObserver(CFRunLoopGetMain(), observer, kCFRunLoopCommonModes);
}
