/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

void nsmstring_append_string_constant(NSMutableString* str) {
  [str appendString:@"hello"];

  for (int i = 0; i < str.length; i++) {
  }
}

void nsmstring_append_string_linear(NSMutableString* str1, NSString* str2) {
  [str1 appendString:str2];

  for (int i = 0; i < str1.length; i++) {
  }
}

void copy_string_constant_FP(NSMutableString* str3) {
  NSMutableString* str1 = [NSMutableString new];
  NSMutableString* str2 =
      [str1 mutableCopy]; // we should do deep copy but we don't
  [str1 appendString:str3];
  for (int i = 0; i < str2.length; i++) {
  }
}
