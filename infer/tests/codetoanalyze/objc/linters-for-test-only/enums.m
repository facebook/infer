/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>

typedef NS_ENUM(NSUInteger, MyName) {
  MyNameUndefined,
  MyNameDefined,
};

int test() { return MyNameUndefined; }

int test_enum_constant_of_enum() { return MyNameDefined; }

enum { RANDOM, IMMEDIATE, SEARCH } strategy;

int test_c_style_enum() { return IMMEDIATE; }
