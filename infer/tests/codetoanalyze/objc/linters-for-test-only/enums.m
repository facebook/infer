/*
 * Copyright (c) 2017 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSObject.h>

typedef NS_ENUM(NSUInteger, MyName) {
  MyNameUndefined,
};

int test() { return MyNameUndefined; }

enum { RANDOM, IMMEDIATE, SEARCH } strategy;

int test_c_style_enum() { return IMMEDIATE; }
