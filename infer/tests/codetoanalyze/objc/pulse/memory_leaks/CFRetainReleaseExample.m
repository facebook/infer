/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <UIKit/UIKit.h>

static CFStringRef use_after_cfrelease_ok(CFStringRef message) {
  CFRetain(message);
  CFStringRef lastMessage = message;
  CFRelease(lastMessage);
  lastMessage = message;
  CFRelease(lastMessage);
  lastMessage = message;

  return lastMessage;
}
