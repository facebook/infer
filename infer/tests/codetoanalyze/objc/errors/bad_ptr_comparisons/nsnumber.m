/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/Foundation.h>

void bad1(NSNumber* isNum) {
  if (isNum) {
  }
}

void bad2(NSNumber* isNum) {
  if (!isNum) {
  }
}

void ok1(NSNumber* isNum) {
  if (isNum != nil) {
  }
}

void ok2(NSNumber* isNum) {
  if (nil != isNum) {
  }
}

void ok3(NSNumber* isNum) {
  if (nil == isNum) {
  }
}

void ok4(NSNumber* isNum) {
  if (isNum == nil) {
  }
}

void accessor_ok1(NSNumber* num) {
  if (![num boolValue]) {
  }
}

void accessor_ok2(NSNumber* num) {
  if ([num boolValue]) {
  }
}
