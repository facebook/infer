/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#import <Foundation/NSData.h>
#import <Foundation/NSObject.h>

@interface A : NSObject

@property(nonatomic) A* child;

@property(nonatomic) A* url;

@end

@implementation A

- (void)testLength:(NSData*)imageData {
  unsigned char* pixels = (unsigned char*)[imageData bytes];

  if (imageData.length > 0) {
    pixels[0] = 255;
  }
}

A* testUrl(A* context) {
  if (context.url) {
    A* entityMemObject = context.child;
    return entityMemObject->_child;
  }
  return nil;
}

@end
