/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/Foundation.h>
#import <stdlib.h>

@interface NSData_models_tests : NSObject

@end

@implementation NSData_models_tests

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

- (NSData*)decodedImageData:(size_t)dataLength {
  void* data = calloc(dataLength, 1);
  return [[NSData alloc] initWithBytesNoCopy:data length:dataLength];
}

- (NSData*)macForIV:(NSData*)IV {
  uint8_t* result = malloc(10);
  return [NSData dataWithBytesNoCopy:result length:10];
}

@end
