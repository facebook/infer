/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
