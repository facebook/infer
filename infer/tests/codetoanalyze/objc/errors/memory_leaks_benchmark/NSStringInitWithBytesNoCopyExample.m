/*
 * Copyright (c) 2014 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

+ (NSData*)randomBytes:(NSUInteger)numOfBytes {
  uint8_t* buffer = malloc(numOfBytes);
  NSData* data = [NSData dataWithBytesNoCopy:buffer length:numOfBytes];
  if (data) {
    return data;
  } else {
    free(buffer);
    return nil;
  }
}

- (NSData*)readDataOfLength:(NSUInteger)length {
  size_t bytesLength = length;
  void* bytes = malloc(bytesLength);
  if (bytes == NULL) {
    return nil;
  }
  return [[NSData alloc] initWithBytesNoCopy:bytes length:5 freeWhenDone:YES];
}

- (NSData*)macForIV:(NSData*)IV {
  uint8_t* result = malloc(10);
  return [NSData dataWithBytesNoCopy:result length:10];
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
