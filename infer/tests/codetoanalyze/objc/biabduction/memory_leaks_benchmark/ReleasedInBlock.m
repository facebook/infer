/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void allocAndReleaseInBlock() {
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
