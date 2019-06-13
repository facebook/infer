/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#import <Foundation/NSObject.h>
#import <Foundation/NSString.h>

@interface NSHTTPCookie : NSObject
@property(nonatomic, readonly, copy) NSString* value;
@end

@implementation NSHTTPCookie
@synthesize value;
- (NSString*)value {
  return value;
}
@end
