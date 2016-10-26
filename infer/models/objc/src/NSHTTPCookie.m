/*
 * Copyright (c) 2015 - present Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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
