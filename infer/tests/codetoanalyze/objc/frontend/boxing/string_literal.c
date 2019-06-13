/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>

NSString* get_string1() {

  return [NSString stringWithUTF8String:"Hello World!"];
}

NSString* get_string2() { return @"Hello World!"; }
