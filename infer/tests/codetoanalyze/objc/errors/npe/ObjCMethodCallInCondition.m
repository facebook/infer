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

@interface CallInConditionA : NSObject

@property(nonatomic) CallInConditionA* child;

@property(nonatomic) CallInConditionA* url;

@end

@implementation CallInConditionA

- (void)testLength:(NSData*)imageData {
  unsigned char* pixels = (unsigned char*)[imageData bytes];

  if (imageData.length > 0) {
    pixels[0] = 255;
  }
}

CallInConditionA* testUrl(CallInConditionA* context) {
  if (context.url) {
    CallInConditionA* entityMemObject = context.child;
    return entityMemObject->_child;
  }
  return nil;
}

@end
