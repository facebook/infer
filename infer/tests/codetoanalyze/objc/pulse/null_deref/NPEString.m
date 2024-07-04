/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

// Test (non-nil) returned values of NSString methods against `nil`
NSString* stringNotNil(NSString* str) {
  if (!str) {
    return [NSString stringWithString:nil];
  }
  return str;
}

NSString* stringWithUTF8String(char* str) {
  NSString* result = [NSString stringWithUTF8String:str];
  return stringNotNil(result);
}

NSString* stringWithUTF8StringBad() { return stringWithUTF8String(NULL); }

NSString* stringWithUTF8StringOk() {
  return stringNotNil(stringWithUTF8String("something"));
}

NSString* stringWithString(NSString* str) {
  NSString* result = [NSString stringWithString:str];
  return stringNotNil(result);
}

NSString* stringWithStringBad() { return stringWithString(nil); }

NSString* attributedStringBad() {
  return [[NSAttributedString alloc] initWithString:nil];
}

NSString* stringWithStringOk() { return stringWithString(@"something"); }

NSString* initWithFormat(NSString* fmt) {
  NSString* result = [[NSString alloc] initWithFormat:fmt];
  return stringNotNil(result);
}

NSString* initWithFormatBad() { return initWithFormat(nil); }

NSString* initWithFormatOk() { return initWithFormat(@"something"); }

NSString* stringWithFormat(NSString* fmt) {
  NSString* result = [NSString stringWithFormat:fmt];
  return stringNotNil(result);
}

NSString* stringWithFormatBad() { return stringWithFormat(nil); }

NSString* stringWithFormatOk() { return stringWithFormat(@"something"); }

NSString* stringWithFormatVariadic(NSString* fmt) {
  NSString* result = [NSString stringWithFormat:fmt, @"foo", @"bar", @"baz"];
  return stringNotNil(result);
}

NSString* stringWithFormatVariadicBad() {
  return stringWithFormatVariadic(nil);
}

NSString* stringWithFormatVariadicOk() {
  return stringWithFormatVariadic(@"something");
}

NSString* stringByAppendingString(NSString* str) {
  NSString* result = @"something";
  result = [result stringByAppendingString:str];
  return stringNotNil(result);
}

NSString* stringByAppendingStringBad() { return stringByAppendingString(nil); }

NSString* stringByAppendingStringOk() {
  return stringByAppendingString(@"something");
}
