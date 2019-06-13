/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSString.h>

NSString* nullableMethod() { return nil; }

NSString* stringWithUTF8StringGood() {
  NSString* str = [NSString stringWithUTF8String:"a"];
  return str;
}

NSString* stringWithUTF8StringBad() {
  NSString* str = [NSString stringWithUTF8String:nullableMethod()];
  return str;
}

NSString* stringWithStringGood() {
  NSString* str = [NSString stringWithString:@"a"];
  return str;
}

NSString* stringWithStringBad() {
  NSString* s = nullableMethod();
  NSString* str = [NSString stringWithString:s];
  return str;
}

NSString* initWithFormatGood() {
  NSString* str = @"a";
  [str initWithFormat:@"a"];
  return str;
}

NSString* initWithFormatBad() {
  NSString* str = @"a";
  NSString* fmt = nil;
  [str initWithFormat:fmt];
  return str;
}

NSString* stringByAppendingStringGood() {
  NSString* str = @"a";
  [str stringByAppendingString:@"b"];
  return str;
}

NSString* stringByAppendingStringBad() {
  NSString* str = @"a";
  [str stringByAppendingString:nil];
  return str;
}
