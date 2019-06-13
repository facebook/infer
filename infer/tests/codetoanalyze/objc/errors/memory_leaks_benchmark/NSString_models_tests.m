/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#import <stdlib.h>

@interface StringInitA : NSObject

@end

@implementation StringInitA

NSString* createURLQueryStringBodyEscaping(NSDictionary* parameters,
                                           NSString* s) {
  NSString* resultString;
  if (s) {
    char* resultBuffer = (char*)malloc(5 * sizeof(char));

    resultString = [s initWithBytesNoCopy:resultBuffer
                                   length:5
                                 encoding:NSUTF8StringEncoding
                             freeWhenDone:YES];
  }
  return resultString;
}

NSString* stringWithUTF8StringNoLeakOk(const char* buf) {
  return [NSString stringWithUTF8String:buf];
}
NSString* stringWithStringNoLeakOk(const char* buf) {
  return [NSString stringWithString:buf];
}

- (void)NSAssertNoLeakOk {
  NSAssert(true, @"This string does not create any memory leak");
}

- (NSString*)hexStringValue {
  size_t hexLen = 2 * 10 * sizeof(char);
  char* outString = (char*)malloc(hexLen + 1);
  NSString* result = (__bridge_transfer NSString*)CFStringCreateWithBytesNoCopy(
      NULL /* default allocator */,
      (const UInt8*)
          outString /* sizeof(char) should always be sizeof(UInt8) on iOS */,
      hexLen,
      kCFStringEncodingASCII /* hex is ASCII */,
      false /* no bom */,
      NULL /* default deallocator -- system takes ownership of bytes */);
  if (result == nil) {
    // On error creating string, we're responsible for freeing outString.
    free(outString);
  }
  return result;
}

@end
