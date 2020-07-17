/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

NSString* mId;
NSCharacterSet* characterSet;

NSString* string_by_appending_same_string_linear_FN(NSString* s) {
  NSString* str = [s stringByAppendingString:@"me"];
  return str;
}

NSString* string_by_appending_string_linear_FN(NSString* s, NSString* m) {
  NSString* str = [s stringByAppendingString:m];
  return str;
}

NSUInteger rangeof_character_from_set_linear_FN(NSString* m) {
  return [m rangeOfString:@"_"].location;
}

NSUInteger rangeof_string_quadratic_FN(NSString* m, NSString* n) {
  return [m rangeOfString:n].location;
}

NSString* substring_from_index_linear_FN() {
  NSUInteger index = rangeof_character_from_set_linear_FN(mId);
  return [mId substringToIndex:index];
}

NSString* has_prefix_constant() {
  NSString* s = @"";
  return [s hasPrefix:s] ? [s substringFromIndex:1] : s;
}

void component_seperated_by_string_linear_FP(NSString* m) {
  NSArray* arrayOfComponents = [m componentsSeparatedByString:@","];
  for (int i = 0; i < arrayOfComponents.count; i++) {
  }
}

void call_component_separated_by_string_constant_FP() {
  NSString* s = @"hello";
  component_seperated_by_string_linear_FP(s);
}

void init_with_bytes_linear_FP(const void* bytes,
                               NSUInteger length,
                               NSStringEncoding encoding) {
  NSString* s = [[NSString alloc] initWithBytes:bytes
                                         length:length
                                       encoding:encoding];
  for (int i = 0; i < s.length; i++) {
  }
}

void init_with_string_constant_FP() {
  NSString* s = @"abcd";
  NSString* str = [[NSString alloc] initWithString:s];
  for (int i = 0; i < str.length; i++) {
  }
}

void init_with_string_linear_FP(NSString* s) {
  NSString* str = [[NSString alloc] initWithString:s];
  for (int i = 0; i < str.length; i++) {
  }
}

void call_init_with_string_constant_FP() {
  NSString* s = [[NSString alloc] init];
  init_with_string_linear_FP(s);
}

void substring_no_end_linear_FP(NSString* s, int x) {
  NSString* sub = [s substringFromIndex:x];
  for (int i = 0; i < sub.length; i++) {
  }
}

void replace_linear_FP(NSString* s) {
  NSString* r = [s stringByReplacingOccurrencesOfString:@"." withString:@","];
  for (int i = 0; i < r.length; i++) {
  }
}

NSString* string_with_utf8_string_linear(const char* p) {
  NSString* s = [NSString stringWithUTF8String:p];
  for (int i = 0; i < [s.length integerValue]; i++) {
  }
  return s;
}

void string_length_linear(NSString* s) {
  for (int i = 0; i < [s.length integerValue]; i++) {
  }
}

bool string_is_equal_to_string_linear(NSString* str1, NSString* str2) {
  return [str1 isEqualToString:str2];
}

NSString* string_by_appending_path_component_linear(NSString* path,
                                                    NSString* file) {
  return [path stringByAppendingPathComponent:file];
}

bool string_has_prefix_linear(NSString* str, NSString* prefix) {
  return [str hasPrefix:prefix];
}
