/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

void anonBlock() {
  CFRunLoopObserverRef observer = CFRunLoopObserverCreateWithHandler(
      NULL,
      kCFRunLoopBeforeWaiting,
      NO,
      UINT_MAX,
      ^(CFRunLoopObserverRef obs, CFRunLoopActivity activity) {
        CFRelease(obs);
      });
  CFRunLoopAddObserver(CFRunLoopGetMain(), observer, kCFRunLoopCommonModes);
}

void blockVar() {
  // This case doesnt fail like the one above (with the anonymous block) used
  // to. That's because using the block variable populates the args list
  // differently thus avoiding the dupliate.
  void (^bvar)(CFRunLoopObserverRef observer, CFRunLoopActivity activity) =
      ^(CFRunLoopObserverRef obs, CFRunLoopActivity activity) {
        CFRelease(obs);
      };

  CFRunLoopObserverRef observer = CFRunLoopObserverCreateWithHandler(
      NULL, kCFRunLoopBeforeWaiting, NO, UINT_MAX, bvar);
  CFRunLoopAddObserver(CFRunLoopGetMain(), observer, kCFRunLoopCommonModes);
}
