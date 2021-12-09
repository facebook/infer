/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

NSString* mId;
NSCharacterSet* characterSet;

NSString* string_by_appending_same_string_linear(NSString* s) {
  NSString* str = [s stringByAppendingString:@"me"];
  for (int i = 0; i < str.length; i++) {
  }
  return str;
}

NSString* string_by_appending_string_linear(NSString* s, NSString* m) {
  NSString* str = [s stringByAppendingString:m];
  for (int i = 0; i < str.length; i++) {
  }
  return str;
}

NSUInteger rangeof_character_from_set_linear(NSString* m) {
  return [m rangeOfString:@"_"].location;
}

NSUInteger rangeof_string_quadratic(NSString* m, NSString* n) {
  return [m rangeOfString:n].location;
}

NSString* has_prefix_constant() {
  NSString* s = @"";
  return [s hasPrefix:s] ? [s substringFromIndex:1] : s;
}

void component_seperated_by_char_linear(NSString* m) {
  NSArray* arrayOfComponents = [m componentsSeparatedByString:@","];
  for (int i = 0; i < arrayOfComponents.count; i++) {
  }
}

void component_seperated_by_string_linear(NSString* m, NSString* sep) {
  NSArray* arrayOfComponents = [m componentsSeparatedByString:sep];
  for (int i = 0; i < arrayOfComponents.count; i++) {
  }
}

void call_component_separated_by_char_constant() {
  NSString* s = @"hello";
  component_seperated_by_char_linear(s);
}

void init_with_bytes_linear(const void* bytes,
                            NSUInteger length,
                            NSStringEncoding encoding) {
  NSString* s = [[NSString alloc] initWithBytes:bytes
                                         length:length
                                       encoding:encoding];
  for (int i = 0; i < s.length; i++) {
  }
}

void init_string_constant() {
  NSString* str = [[NSString alloc] init];
  for (int i = 0; i < str.length; i++) {
  }
}

void init_with_string_constant() {
  NSString* s = @"abcd";
  NSString* str = [[NSString alloc] initWithString:s];
  for (int i = 0; i < str.length; i++) {
  }
}

void init_with_string_linear(NSString* s) {
  NSString* str = [[NSString alloc] initWithString:s];
  for (int i = 0; i < str.length; i++) {
  }
}

void call_init_with_string_constant() {
  NSString* s = [[NSString alloc] init];
  init_with_string_linear(s);
}

void substring_no_end_linear(NSString* s, int x) {
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

void attributedstring_length_linear(NSAttributedString* s) {
  for (int i = 0; i < s.length; i++) {
  }
}

void call_string_length_linear(NSAttributedString* s) {
  string_length_linear(s.string);
}

void enumerateAttribute_quadratic_FN(NSAttributedString* attributedString,
                                     NSString* kCTFontAttributeName,
                                     int x) {
  [attributedString
      enumerateAttribute:kCTFontAttributeName
                 inRange:NSMakeRange(0, [attributedString length])
                 options:
                     NSAttributedStringEnumerationLongestEffectiveRangeNotRequired
              usingBlock:^(id value, NSRange range, BOOL* stop) {
                for (int i = 0; i <= x; i++) {
                }
              }];
}

void enumerateAttribute_linear(NSAttributedString* attributedString,
                               NSString* kCTFontAttributeName,
                               int x) {
  [attributedString
      enumerateAttribute:kCTFontAttributeName
                 inRange:NSMakeRange(0, [attributedString length])
                 options:
                     NSAttributedStringEnumerationLongestEffectiveRangeNotRequired
              usingBlock:^(id value, NSRange range, BOOL* stop) {
                int p = 0;
              }];
}

void enumerateAttribute_via_block_captured_linear(NSArray* array, int x) {
  __block BOOL answer = NO;
  [array enumerateObjectsUsingBlock:^(id obj, NSUInteger index, BOOL* stop) {
    answer = YES;
  }];
}

@interface DummyClass : NSObject
@end

@implementation DummyClass

+ (void)call_string_by_appending_string_constant {
  NSString* s = [NSStringFromClass(self) stringByAppendingString:@"abc"];
}

@end
