/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <Foundation/Foundation.h>

@interface CStringUsingEncodingTest : NSObject

@end

@implementation CStringUsingEncodingTest

- (void)cStringUsingEncoding_no_copy_bad:(NSString*)name {
  char* encoding = (char*)[name cStringUsingEncoding:NSUTF8StringEncoding];
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c = encoding;
  });
}

- (void)cStringUsingEncoding_copy_good:(NSString*)name {
  char* encoding = (char*)[name cStringUsingEncoding:NSUTF8StringEncoding];
  char* encoding_copy = "";
  strcpy(encoding_copy, encoding);
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c = encoding_copy;
  });
}

- (void)UTF8String_no_copy_bad:(NSString*)name {
  char* utf8string = name.UTF8String;
  dispatch_async(dispatch_get_main_queue(), ^{
    const char* c = utf8string;
  });
}

@end
